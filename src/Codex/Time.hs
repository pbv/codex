{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}

module Codex.Time (
    Time,    -- ^ export abstract types
    TimeEnv,
    makeTimeEnv,
    Constraint(..),
    early, late,  lower, higher,
    fromLocalTime,
    fromUTCTime,
    timeLeft, 
    parseTime,
    parseConstraint,
    evalTime,
    evalConstraint,
    showTime,
    formatNominalDiffTime
    ) where 

import           Data.Char
import           Data.List (intersperse)
import           Data.Maybe (catMaybes)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time hiding (parseTime)

import           Text.ParserCombinators.ReadP


type Name = Text             -- event names

-- | time expressions
data Time 
  = Event Name               -- ^ named event 
  | Local LocalTime          -- ^ local time constant
  | UTC UTCTime              -- ^ unversal time constant
  | Add Diff Time            -- ^ add a time difference
  deriving (Eq, Read, Show)

data Diff = Diff !Int !Unit
  deriving (Eq, Read, Show)

data Unit
  = Week
  | Day
  | Hour
  | Minute
  | Second
  deriving (Eq, Read, Show)


fromLocalTime :: LocalTime -> Time
fromLocalTime = Local

fromUTCTime :: UTCTime -> Time
fromUTCTime = UTC

-- | submission validity constraints;
-- parametrized by the type for time
data Constraint t
  = OK           -- ^ no constraint
  | Before t     -- ^ before some time 
  | After t      -- ^ after some time
  | MaxAttempts Int  -- ^ maximum number of submissions
  | And (Constraint t) (Constraint t)  -- ^ conjunction
  deriving (Eq, Read, Show, Functor, Foldable, Traversable)


early :: Ord t => t -> Constraint t -> Bool
early t c = case lower c of
              Nothing -> False
              Just t' -> t < t'

{-
early t (And c1 c2) = early t c1 || early t c2
early t (After t')  = t < t'
early t _           = False
-}
              
late :: Ord t => t -> Constraint t -> Bool
late t c = case higher c of
             Nothing -> False
             Just t' -> t > t'

{-
late t (And c1 c2) = late t c1 || late t c2
late t (Before t')  = t > t'
late t _           = False
-}


-- | lower and higher time required by a constraint
lower :: Ord t => Constraint t -> Maybe t
lower (After t) = Just t 
lower (And c1 c2)
  = case catMaybes [lower c1, lower c2] of
      [] -> Nothing
      ts -> Just (maximum ts)
lower _ = Nothing

higher :: Ord t => Constraint t -> Maybe t
higher (Before t)  = Just t 
higher (And c1 c2)
  = case catMaybes [higher c1, higher c2] of
      [] -> Nothing
      ts -> Just (minimum ts)
higher _ = Nothing


timeLeft :: UTCTime -> Constraint UTCTime -> Maybe NominalDiffTime
timeLeft t c = fmap (\t'-> diffUTCTime t' t) (higher c)


{-
-- | interval parameterized by time 
data Interval t
  = Interval { lower :: !(Maybe t)
             , higher :: !(Maybe t)
             }
    deriving (Eq, Read, Show, Functor, Traversable, Foldable)

-- | timing relation between a time and an interval
data Timing
  = Early | Valid | Overdue
  deriving (Eq, Ord, Read, Show, Typeable)
-}

--
-- | parse a time constraint
--
parseConstraint :: String -> Maybe (Constraint Time)
parseConstraint str
  = case readP_to_S (skipSpaces >> parseConstraintP) str of
       ((v, ""):_) -> Just v
       _           -> Nothing


-- | worker 
parseConstraintP :: ReadP (Constraint Time)
parseConstraintP = (parseBaseP >>= cont) <++ return OK
  where cont b = do symbol "and"; b'<-parseBaseP; cont (And b b')
                 <++ return b

parseBaseP =
  do (symbol "before" <++ symbol "until"); Before <$> parseTimeP
  <++
  do symbol "after"; After <$> parseTimeP
  <++
  do symbol "attempts"; MaxAttempts <$> integer


-- | parse time expressions
parseTime :: String -> Maybe Time
parseTime str
  = case readP_to_S (skipSpaces >> parseTimeP) str of
      ((t, ""):_) -> Just t
      _           -> Nothing

-- | worker function
parseTimeP :: ReadP Time
parseTimeP = base >>= cont
  where
    base = (Event <$> eventName) <++ (Local <$> localTime)
    cont t = do
      d <- timeDiff
      cont (Add d t)
      <++
      return t

  -- t  <- base
  -- ds <- many' timeDiff
  -- return (foldr Add t ds)
  -- where
    

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
timeDiff :: ReadP Diff
timeDiff = do
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


-- | helper functions

-- | non-backtracking versions of the parsing combinators
-- many', many1' :: ReadP a -> ReadP [a]
-- many1' p = do x<-p; xs<-many' p; return (x:xs)
-- many' p = many1' p <++ return []


-- | ignore trailing spaces
--
token :: ReadP a -> ReadP a
token p = do v<-p; skipSpaces; return v

integer :: ReadP Int
integer = token (readS_to_P reads) 

symbol :: String -> ReadP String
symbol s = token (string s)

-----------------------------------------------------------
---- semantics
-----------------------------------------------------------

-- | environments for evaluation of time expression 
data TimeEnv
  = TimeEnv
    { timeZone :: TimeZone
    , timeEvents :: Name -> Maybe Time
    }


makeTimeEnv :: TimeZone -> (Name -> Maybe Time) -> TimeEnv
makeTimeEnv = TimeEnv


-- | evaluate a time expression;
-- top-level wrapper
evalTime :: TimeEnv -> Time -> Either Text UTCTime
evalTime env t = evalTime' env [] t

-- | worker function
evalTime' :: TimeEnv -> [Name] -> Time -> Either Text UTCTime
evalTime' env@TimeEnv{..} deps (Event name)
  | name `elem` deps =
      Left ("cyclic dependency: " <>
            T.concat (intersperse "->" $ reverse (name:deps)))
  | otherwise =
      case timeEvents name of
        Nothing -> Left ("undefined event: " <> name)
        Just t -> evalTime' env (name:deps) t
evalTime' _ _ (UTC t)
  = pure t
evalTime' TimeEnv{..} _ (Local t)
  = pure (localTimeToUTC timeZone t)     
evalTime' env deps (Add d e)
  = addUTCTime (evalDiff d) <$> evalTime' env deps e


evalDiff :: Diff -> NominalDiffTime
evalDiff (Diff value un) = fromIntegral value * toSeconds un
  where
    toSeconds Second = 1
    toSeconds Minute = 60
    toSeconds Hour   = 3600
    toSeconds Day    = 24*3600
    toSeconds Week   = 7*24*3600
  

--
-- | evaluate a submission constraint
--
evalConstraint :: TimeEnv
               -> Constraint Time
               -> Either Text (Constraint UTCTime)
evalConstraint env = traverse (evalTime env)  


-------------------------------------------------------------
-- pretty printing
-------------------------------------------------------------

showTime :: TimeZone -> UTCTime -> Text
showTime tz = T.pack . formatTime defaultTimeLocale "%c"  . utcToLocalTime tz

-- format a time difference
formatNominalDiffTime :: NominalDiffTime -> String
formatNominalDiffTime secs
  | secs>=0  = unwords $
               [show d ++ " dias" | d>0] ++
               [show (h`rem`24) ++ " horas" | h>0] ++
               [show (m`rem`60) ++ " mins" | m>0] ++
               ["<1 min" | secs<60]
  | otherwise = "--/--"
  where m = floor (secs / 60) :: Int
        h = m `div` 60
        d = h `div` 24
