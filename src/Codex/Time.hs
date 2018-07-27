{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}

module Codex.Time (
    Time,
    Events,
    Interval(..),
    Timing(..),
    fromLocalTime,
    fromUTCTime,
    parseTime,
    parseInterval,
    evalTime,
    evalInterval,
    timeInterval,
    showTime,
    formatNominalDiffTime
    ) where 

import           Data.Char
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Typeable
import           Data.Time hiding (parseTime)

import           Text.ParserCombinators.ReadP


type Name = Text             -- event names

-- | time expressions
data Time 
  = Event Name               -- event 
  | Local LocalTime          -- time constants
  | UTC UTCTime             
  | Add Diff Time            -- add a time difference
  deriving (Eq, Show)

data Diff = Diff !Int !Unit
  deriving (Eq, Show)

data Unit
  = Week
  | Day
  | Hour
  | Minute
  | Second
  deriving (Eq, Show)


fromLocalTime :: LocalTime -> Time
fromLocalTime = Local

fromUTCTime :: UTCTime -> Time
fromUTCTime = UTC


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


--
-- | parse an interval and a time expression
--
parseInterval :: String -> Maybe (Interval Time)
parseInterval str
  = case readP_to_S (skipSpaces >> parseIntervalP) str of
       ((v, ""):_) -> Just v
       _           -> Nothing


-- | worker 
parseIntervalP :: ReadP (Interval Time)
parseIntervalP =
  do symbol "always"
     return (Interval Nothing Nothing)
  <++
  do symbol "until"
     t <- parseTimeP
     return (Interval Nothing (Just t))
  <++
  do symbol "after"
     t <- parseTimeP
     return (Interval (Just t) Nothing)
  <++
  do symbol "between"
     start <- parseTimeP
     symbol "and"
     end <- parseTimeP
     return (Interval (Just start) (Just end))

-- | parse time expressions
parseTime :: String -> Maybe Time
parseTime str
  = case readP_to_S (skipSpaces >> parseTimeP) str of
      ((t, ""):_) -> Just t
      _           -> Nothing

-- | worker function
parseTimeP :: ReadP Time
parseTimeP = do
  t  <- base
  ds <- many' timeDiff
  return (foldr Add t ds)
  where
    base = (Event <$> eventName) <++ (Local <$> localTime)

-- | parse local time strings
localTime ::  ReadP LocalTime
localTime =
  readPTime True defaultTimeLocale "%H:%M %d/%m/%0Y"
  <++
  readPTime True defaultTimeLocale "%d/%m/%0Y %H:%M"
  <++
  readPTime True defaultTimeLocale "%d/%m/%0Y"

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
many', many1' :: ReadP a -> ReadP [a]
many1' p = do x<-p; xs<-many' p; return (x:xs)
many' p = many1' p <++ return []

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

-- | environments for event names
type Events = Name -> Maybe Time

-- | evaluate a time
evalTime :: TimeZone -> Events -> Time -> Either String UTCTime
evalTime tz env t = evalTime' tz env [] t

-- | worker function
evalTime' :: TimeZone -> Events -> [Name] -> Time -> Either String UTCTime
evalTime' tz env viz (Event name)
  | name `elem` viz = Left ("cyclic dependency: " ++ show (name:viz))
  | otherwise = case env name of
      Nothing -> Left ("undefined event or parse error: " ++ show name)
      Just t -> evalTime' tz env (name:viz) t
evalTime' tz _ _ (UTC t)
  = pure t
evalTime' tz _ _ (Local t)
  = pure (localTimeToUTC tz t)     
evalTime' tz env viz (Add d e)
  = addUTCTime (evalDiff d) <$> evalTime' tz env viz e


evalDiff :: Diff -> NominalDiffTime
evalDiff (Diff value un) = fromIntegral value * toSeconds un
  where
    toSeconds Second = 1
    toSeconds Minute = 60
    toSeconds Hour   = 3600
    toSeconds Day    = 24*3600
    toSeconds Week   = 7*24*3600
  


-- | evaluate intervals
evalInterval :: TimeZone -> Events -> Interval Time
             -> Either String (Interval UTCTime)
evalInterval tz events = traverse (evalTime tz events)

{-
evalI' :: Monad m => (t -> m t') -> Interval t -> m (Interval t')
evalI' f (Interval (Just l) (Just h)) = do
    l' <- f l
    h' <- f h
    return (Interval (Just l') (Just h'))
evalI' f (Interval (Just l) Nothing) = do
    l' <- f l
    return (Interval (Just l') Nothing)
evalI' f (Interval Nothing (Just h)) = do
      h' <- f h
      return (Interval Nothing (Just h'))
evalI' _ _  =
      return (Interval Nothing Nothing)
-}

--
-- | relation between a point and an interval
--
timeInterval :: Ord t => t -> Interval t -> Timing
timeInterval t (Interval (Just low) (Just high))
  | t<low     = Early
  | t>high    = Overdue
  | otherwise = Valid
timeInterval t (Interval (Just low) Nothing)
  = if t<low then Early else Valid
timeInterval t (Interval Nothing (Just high))
  = if t<=high then Valid else Overdue
timeInterval _ _
  = Valid


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
