{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveFunctor #-}

{-
  Policies for classifying submissions as valid or invalid
-}

module Codex.Policy  where

import           Data.Char
import qualified Data.Text as T
import           Data.Time 
import           Data.Maybe (isJust)

import qualified Data.HashMap.Strict as HM
import qualified Data.Configurator as Configurator
import qualified Data.Configurator.Types as Configurator

import           Text.ParserCombinators.ReadP

import           Control.Monad.Reader
import           Control.Monad.Writer
import           Control.Monad.State
import           Control.Monad.Except

import           Codex.Types
import           Codex.Page
import           Codex.Application
import           Codex.Submission
import           Codex.Utils

import qualified Snap.Snaplet.SqliteSimple as S
import           Heist.Splices     as I
import qualified Heist.Interpreted as I
import           Data.Map.Syntax


-- | type for policy restrictions

data Policy t
  = Policy
    { maxAttempts :: Maybe Int
    , timeConstraint :: Constr' t
    } deriving (Show, Functor)


type Constr' t = Either Text (Constr t)
   -- ^ constraint (Right) or an error messsage (Left)

-- | time constraints 
data Constr t
  = Before t                   -- ^ before this time
  | After t                    -- ^ after this time
  | Conj (Constr t) (Constr t) -- ^ conjunction
  | Disj (Constr t) (Constr t) -- ^ disjunction
  | Always
  | Never
  deriving (Show, Functor)

-- | time expressions
data Time
  = Event Name               -- ^ named event 
  | Local LocalTime          -- ^ local time constant
  | AddDiff Time Diff        -- ^ add a time difference
  deriving (Eq, Read, Show)

type Name = Text             -- event names

data Diff = Diff !Int !Unit
  deriving (Eq, Read, Show)

data Unit
  = Week
  | Day
  | Hour
  | Minute
  | Second
  deriving (Eq, Read, Show)


emptySubmission :: UserLogin -> FilePath -> UTCTime -> Submission
emptySubmission user path time
  = Submission undefined user path time undefined undefined undefined

-------------------------------------------------------------------------
-- geting policy from metadata
-------------------------------------------------------------------------
getPolicy :: Page -> Policy Time
getPolicy page =
  Policy { maxAttempts = lookupFromMeta "attempts" meta
         , timeConstraint = constr'
         }
  where
    meta = pageMeta page
    constr' = case lookupFromMeta "valid" meta of
                Nothing -> Right Always
                Just txt -> parseConstraint txt


--------------------------------------------------------------------------          
-- Semantics 
---------------------------------------------------------------------------

-- | environments
data TimeEnv
  = TimeEnv
    { timeZone :: TimeZone
    , timeEvents :: Name -> Maybe Time
    }


-- | evaluate a time expression; wrapper
evalTime :: MonadError Text m => TimeEnv -> Time -> m UTCTime
evalTime TimeEnv{..} t = worker [] t
  where
    worker deps (Event name)
      | name `elem` deps =
          throwError ("cyclic dependency: " <> T.pack (show name))
      | otherwise =
          case timeEvents name of
            Just t -> worker (name:deps) t
            Nothing -> throwError ("undefined event: " <> T.pack (show name))
    worker _ (Local lt)
      = return (localTimeToUTC timeZone lt)
    worker deps (AddDiff e d)
      = addUTCTime (evalDiff d) <$> worker deps e


evalDiff :: Diff -> NominalDiffTime
evalDiff (Diff value un) = fromIntegral value * toSeconds un
  where
    toSeconds Second = 1
    toSeconds Minute = 60
    toSeconds Hour   = 3600
    toSeconds Day    = 24*3600
    toSeconds Week   = 7*24*3600


-- | simplify a time constraint
normalizeConstr' :: MonadError Text m
                 => TimeEnv -> Constr' Time -> m (Constr UTCTime)
normalizeConstr' _   (Left msg) = throwError msg
normalizeConstr' env (Right constr) = go constr
  where
    go Always
      = pure Always
    go Never
      = pure Never
    go (Before t)
      = Before <$> evalTime env t
    go (After t)
      = After <$> evalTime env t
    go (Conj c1 c2)
      = Conj <$> go c1 <*> go c2
    go (Disj c1 c2)
      = Disj <$> go c1 <*> go c2


-- | evaluate a time constraint
evalConstr :: MonadWriter [Text] m
           => TimeEnv -> UTCTime -> Constr UTCTime -> m Bool
evalConstr TimeEnv{..} timeNow constr = go constr
  where
    go Always
      = pure True
    go Never
      = pure False
    go (Before limit) = do
      unless (timeNow <= limit) $ do
        tell ["Submissions until " <> formatLocalTime timeZone limit]
      return (timeNow <= limit)
    go (After limit) = do
      unless (timeNow >= limit) $ do
        tell ["Submissions from " <> formatLocalTime timeZone limit]
      return (timeNow >= limit)
    go (Conj c1 c2) = do
      r <- go c1
      if r then go c2 else return False
    go (Disj c1 c2) = do
      r <- go c1
      if r then return True else go c2


checkMaxAttempts :: ( MonadError Text m, S.HasSqlite m )
                 => Submission
                 -> Int
                 -> m ()
checkMaxAttempts Submission{..} limit = do
  count <- countEarlier submitUser submitPath submitTime
  unless (count < limit) $
    throwError ("Maximum " <> T.pack (show limit) <> " attempts exceeded")


checkConstr' :: MonadError Text m
             => TimeEnv -> UTCTime -> Constr' Time -> m ()
checkConstr'  env time constr'
  = case normalizeConstr' env constr' of
      Left msg -> throwError msg
      Right constr -> case runWriter (evalConstr env time constr) of
                         (True,  _) -> pure ()
                         (False, msgs) -> throwError (T.unlines msgs)


checkPolicy :: ( S.HasSqlite m )
            => TimeEnv
            -> Policy Time
            -> Submission
            -> m Validity
checkPolicy env policy submission = do
  result <- runExceptT $ checkPolicy' env policy submission
  return $ case result of
             Left msg -> Invalid msg
             Right _  -> Valid

checkPolicy' :: (S.HasSqlite m, MonadError Text m)
             => TimeEnv -> Policy Time -> Submission -> m ()
checkPolicy' env Policy{..} Submission{..} = do
  case maxAttempts of
    Nothing -> pure ()
    Just limit -> checkMaxAttempts Submission{..} limit
  checkConstr' env submitTime timeConstraint


instance S.HasSqlite m => S.HasSqlite (ExceptT e m) where
  getSqliteState = lift S.getSqliteState


-- | get event mapping from a config file
----------------------------------------------------------------
getTimeEnv :: Codex TimeEnv
getTimeEnv = do
  tz <- liftIO getCurrentTimeZone
  evcfg <- gets _eventcfg
  hm <- liftIO $ Configurator.getMap evcfg
  let hm' = HM.map (\v -> Configurator.convert v >>= parseTime) hm
  return TimeEnv { timeZone = tz
                 , timeEvents = \k -> join (HM.lookup k hm')
                 }


-------------------------------------------------------------------------------
-- parsing
-------------------------------------------------------------------------------
parseConstraint :: String -> Either Text (Constr Time)
parseConstraint str
  = case readP_to_S (skipSpaces >> parseConstraintP) str of
       ((c, ""):_) -> return c
       ((_, txt):_) -> throwError ("can't parse constraint: " <> T.pack (show txt))
       _   -> throwError "can't parse constraint"


-- | parse a list of constraints
parseConstraintP :: ReadP (Constr Time)
parseConstraintP = chainl1' parseBaseP parseOperator

parseOperator = do { symbol "and"; return Conj }
                <++
                do { symbol "or"; return Disj }


-- | parse a single constraint
parseBaseP :: ReadP (Constr Time)
parseBaseP =
  do (symbol "before" <++ symbol "until"); Before <$> parseTimeP
  <++
  do (symbol "after" <++ symbol "from"); After <$> parseTimeP
  <++
  between (symbol "(") (symbol ")") parseConstraintP



-- | parse time expressions; top-level wrapper funcion
parseTime :: String -> Maybe Time
parseTime str
  = case readP_to_S (skipSpaces >> parseTimeP) str of
      ((t, ""):_) -> Just t
      _           -> Nothing

-- worker function
parseTimeP :: ReadP Time
parseTimeP = base >>= cont
  where
    base = (Event <$> eventName) <++ (Local <$> localTime)
    cont t = do
      d <- parseTimeDiff
      cont (AddDiff t d)
      <++
      return t



-- | parse local time strings
localTime ::  ReadP LocalTime
localTime = token (readPTime True defaultTimeLocale "%H:%M %d/%m/%0Y"
                   <++
                   readPTime True defaultTimeLocale "%d/%m/%0Y %H:%M"
                   <++
                   readPTime True defaultTimeLocale "%d/%m/%0Y")

-- | parse an event
--
eventName :: ReadP Name
eventName = token $ do
   char '#'
   x <- satisfy isAlpha
   xs<- munch (\c -> isAlphaNum c || c == '_')
   return (T.pack (x:xs))

-- | parse a time difference
--
parseTimeDiff :: ReadP Diff
parseTimeDiff = do
  s <- sign
  n <- integer
  u <- unit
  return (Diff (s n) u)

sign :: ReadP (Int -> Int)
sign = do {token (char '+'); return id}
       <++
       do {token (char '-'); return negate}

unit :: ReadP Unit
unit = do
  do (symbol "week" <++ symbol "w"); return Week
  <++
  do (symbol "day" <++ symbol "d"); return Day
  <++
  do (symbol "hour" <++ symbol "h"); return Hour
  <++
  do (symbol "minute" <++ symbol "m"); return Minute
  <++
  do (symbol "second" <++ symbol "s"); return Second



-- | ignore trailing spaces
--
token :: ReadP a -> ReadP a
token p = do v<-p; skipSpaces; return v

integer :: ReadP Int
integer = token (readS_to_P reads)

symbol :: String -> ReadP String
symbol s = token (string s)


-- Like 'chainl1', but no backtracking
chainl1' :: ReadP a -> ReadP (a -> a -> a) -> ReadP a
chainl1' p op = p >>= rest
  where rest x = do f <- op
                    y <- p
                    rest (f x y)
                 <++ return x




-------------------------------------------------------------
-- pretty printing
-------------------------------------------------------------
prettyConstr Always = "open"
prettyConstr Never = "closed"
prettyConstr (After t) = "after " <> t
prettyConstr (Before t) = "before " <> t
prettyConstr (Conj c1 c2)
  = prettyConstr c1 <> " and " <> prettyConstr c2
prettyConstr (Disj c1 c2)
  = prettyConstr c1 <> " or " <> prettyConstr c2



-- format an UTC time as local time
formatLocalTime :: TimeZone -> UTCTime -> Text
formatLocalTime tz
  = T.pack . formatTime defaultTimeLocale "%c" . utcToLocalTime tz

-- | format a time difference
formatNominalDiffTime :: NominalDiffTime -> Text
formatNominalDiffTime secs
  | secs>0  = T.pack $ unwords $
              [shows d "d" | d>0] ++
              [shows (h`rem`24) "h" | h>0] ++
              [shows (m`rem`60) "m" | m>0] ++
              ["<1 m" | secs<60]
  | otherwise = "--/--"
  where m = floor (secs / 60) :: Int
        h = m `div` 60
        d = h `div` 24


-------------------------------------------------------------
-- splices for policy checks
------------------------------------------------------------
policySplices :: Page
              -> UserLogin
              -> FilePath
              -> Codex ISplices
policySplices page submitUser submitPath = do
  let policy@Policy{..} = getPolicy page
  let lock = pageLockInvalid page
  env <- getTimeEnv
  now <- liftIO getCurrentTime
  count <- countEarlier submitUser submitPath now
  let dummy = emptySubmission submitUser submitPath now
  check <- checkPolicy env policy dummy
  let constrText
        = case normalizeConstr' env timeConstraint of
            Left msg -> msg
            Right constr -> prettyConstr
                              (formatLocalTime (timeZone env) <$> constr)
  return $ do
    "timing" ## I.textSplice constrText
    "if-available" ## I.ifElseISplice (check == Valid || not lock)
    "if-max-attempts" ## I.ifElseISplice (isJust maxAttempts)
    "submissions-attempts" ## I.textSplice (T.pack $ show count)
    "max-attempts" ## case maxAttempts of
      Nothing -> return []
      Just limit -> I.textSplice (T.pack $ show limit)
    "submissions-remain" ## case maxAttempts of
      Nothing -> return []
      Just limit -> let remain = max 0 (limit-count)
                    in I.textSplice (T.pack $ show remain)

