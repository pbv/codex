{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}

{-
-- Policies for classifying submissions as valid or invalid
-}

module Codex.Policy (
    TimeExpr,
    TimeEnv,
    makeTimeEnv,
    Policy(..),
    early, late, lower, higher, maxAttempts,
    fromLocalTime,
    fromUTCTime,
    timeLeft, 
    parseTimeExpr,
    parsePolicy,
    evalTimeExpr,
    evalPolicy,
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
data TimeExpr 
  = Event Name               -- ^ named event 
  | Local LocalTime          -- ^ local time constant
  | UTC UTCTime              -- ^ unversal time constant
  | Add Diff TimeExpr        -- ^ add a time difference
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


fromLocalTime :: LocalTime -> TimeExpr
fromLocalTime = Local

fromUTCTime :: UTCTime -> TimeExpr
fromUTCTime = UTC

-- | policyt constraints for submissions;
-- parametrized by the type for time values
data Policy time
  = OK           -- ^ no constraint
  | Before time  -- ^ before some time 
  | After time   -- ^ after some time
  | MaxAttempts Int  -- ^ less than a maximum number of submissions
  | And (Policy time) (Policy time)  -- ^ conjunction
  deriving (Eq, Read, Show, Functor, Foldable, Traversable)


early :: Ord time => time -> Policy time -> Bool
early t c = maybe False (\l -> t<l) (lower c)
           
late :: Ord time => time -> Policy time -> Bool
late t c = maybe False (\h -> t>h) (higher c)


-- | lower and higher time required by a constraint
lower :: Ord time => Policy time -> Maybe time
lower (After t) = Just t 
lower (And c1 c2)
  = case catMaybes [lower c1, lower c2] of
      [] -> Nothing
      ts -> Just (maximum ts)
lower _ = Nothing

higher :: Ord time => Policy time -> Maybe time
higher (Before t)  = Just t 
higher (And c1 c2)
  = case catMaybes [higher c1, higher c2] of
      [] -> Nothing
      ts -> Just (minimum ts)
higher _ = Nothing


timeLeft :: UTCTime -> Policy UTCTime -> Maybe NominalDiffTime
timeLeft t c = fmap (\t'-> diffUTCTime t' t) (higher c)


maxAttempts :: Policy time -> Maybe Int
maxAttempts (MaxAttempts n) = Just n
maxAttempts (And c1 c2) 
  = case catMaybes [maxAttempts c1, maxAttempts c2] of
      [] -> Nothing
      xs -> Just (minimum xs)
maxAttempts _ = Nothing


--
-- | parse a policy constraint
--
parsePolicy :: String -> Maybe (Policy TimeExpr)
parsePolicy str
  = case readP_to_S (skipSpaces >> parseConstraintP) str of
       ((v, ""):_) -> Just v
       _           -> Nothing

-- worker function
parseConstraintP :: ReadP (Policy TimeExpr)
parseConstraintP = (parseBaseP >>= cont) <++ return OK
  where cont b = do symbol "and"; b'<-parseBaseP; cont (And b b')
                 <++ return b

parseBaseP =
  do (symbol "before" <++ symbol "until"); Before <$> parseTimeP
  <++
  do symbol "after"; After <$> parseTimeP
  <++
  do symbol "attempts"; MaxAttempts <$> integer


-- | parse time expressions; wrapper
parseTimeExpr :: String -> Maybe TimeExpr
parseTimeExpr str
  = case readP_to_S (skipSpaces >> parseTimeP) str of
      ((t, ""):_) -> Just t
      _           -> Nothing

-- worker function
parseTimeP :: ReadP TimeExpr
parseTimeP = base >>= cont
  where
    base = (Event <$> eventName) <++ (Local <$> localTime)
    cont t = do
      d <- timeDiff
      cont (Add d t)
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

-- | environments for evaluation of time expressions 
data TimeEnv
  = TimeEnv
    { timeZone :: TimeZone
    , timeEvents :: Name -> Maybe TimeExpr
    }


makeTimeEnv :: TimeZone -> (Name -> Maybe TimeExpr) -> TimeEnv
makeTimeEnv = TimeEnv


-- | evaluate a time expression; wrapper
evalTimeExpr :: TimeEnv -> TimeExpr -> Either Text UTCTime
evalTimeExpr env t = evalTime' env [] t

-- worker function
evalTime' :: TimeEnv -> [Name] -> TimeExpr -> Either Text UTCTime
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
-- | evaluate a policy
--
evalPolicy :: TimeEnv
           -> Policy TimeExpr
           -> Either Text (Policy UTCTime)
evalPolicy env = traverse (evalTimeExpr env)  


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
