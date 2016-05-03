
{-# LANGUAGE DeriveDataTypeable #-}

module Interval where

import           Data.Char
import           Data.Maybe
import           Data.Typeable
import           Data.Time
import           Data.Time.LocalTime
import           System.Locale
import           Control.Applicative

import           Text.ParserCombinators.ReadP


-- abstract syntax

-- | a time interval
data Interval = Always
              | Until !TimeExpr
              | After !TimeExpr
              | Between !TimeExpr !TimeExpr
              deriving (Eq, Show)

-- | a time expression
data TimeExpr = Absolute !UTCTime
              | Event !String
              | Add !NominalDiffTime !TimeExpr
          deriving (Eq, Show)


-- | timing qualifications
data Timing = OnTime   -- ok
            | Early    -- too early
            | Overdue  -- too late
            deriving (Eq, Read, Show, Typeable)



from, until :: Interval -> Maybe TimeExpr
from (After e)     = Just e
from (Between e _) = Just e
from _             = Nothing

until (Until e)      = Just e
until (Between _ e)  = Just e
until _              = Nothing

-- parse intervals

readInterval :: ZonedTime -> String -> Maybe Interval
readInterval t txt
  = let tz = zonedTimeZone t
        loc= zonedTimeToLocalTime t
        parseI = parseInterval tz loc
    in
     case readP_to_S parseI txt of
       ((i, ""):_) -> Just i
       _ -> Nothing

parseInterval :: TimeZone -> LocalTime -> ReadP Interval
parseInterval tz now =
  do token "always"
     return Always
  <++
  do token "until"
     Until <$> parseTimeExpr tz now
  <++
  do token "after"
     After <$> parseTimeExpr tz now
  <++
  do token "between"
     start <- parseTimeExpr tz now
     token "and"
     end <- parseTimeExpr tz now
     return (Between start end)
  
     
-- parse time expressions
parseTimeExpr :: TimeZone  -> LocalTime -> ReadP TimeExpr
parseTimeExpr tz now
  = do expr <- base
       (do {diff <- parseDiff; return (Add diff expr)}
        <++ return expr)
  where
    base = Absolute <$> parseUTCTime tz now
           <|>
           Event <$> (skipSpaces >> parseName)
 

-- parse time strings
parseLocalTime :: LocalTime -> ReadP LocalTime
parseLocalTime now  = 
  readS_to_P (readsTime defaultTimeLocale "%H:%M %d/%m/%0Y")
   <++
   readS_to_P (readsTime defaultTimeLocale "%d/%m/%0Y")
   <++
   (readS_to_P (readsTime defaultTimeLocale "%H:%M") >>= \t ->
     return t { localDay = localDay now })
  

parseUTCTime :: TimeZone  -> LocalTime -> ReadP UTCTime
parseUTCTime tz  now  = localTimeToUTC tz <$> parseLocalTime now

parseName :: ReadP String
parseName = do
  x <- satisfy isAlpha
  xs<- munch isAlphaNum
  return (x:xs)
  

parseDiff :: ReadP NominalDiffTime
parseDiff 
  = do s <- skipSpaces >> parseSign
       ds <- many1' diff
       return (s * sum ds)
  where diff =  do
          x <- readS_to_P reads :: ReadP Int
          u <- skipSpaces >> parseUnit
          return (fromIntegral x * u)

parseSign = do char '+'; return 1
            <|> do char '-'; return (-1)

parseUnit :: ReadP NominalDiffTime
parseUnit = do char 'd'; return (24*3600)   -- 1 day
            <|> do char 'h'; return 3600    -- 1 hour
            <|> do char 'm'; return 60      -- 1 minute
            <|> do char 's'; return 1       -- 1 second
             


-- non-backtracking versions of the parsing combinators
many', many1' :: ReadP a -> ReadP [a]
many1' p = do x <- p; xs <-many' p; return (x:xs)
many' p = many1' p <++ return []


-- read a string, ignoring leading white space
token :: String -> ReadP String
token s = skipSpaces >> string s


-- * semantics
-- | a  environment for events
type Events = [(String, UTCTime)]

-- | time value of an expression
timeVal :: Events -> TimeExpr -> Maybe UTCTime
timeVal env (Absolute t) = return t
timeVal env (Event n)    = lookup n env
timeVal env (Add d e)    = addUTCTime d <$> timeVal env e

{-
-- | inside a time interval
inside :: Events -> Interval -> UTCTime -> Bool
inside env Always    t = True
inside env (Until e) t
  = maybe False (t <) (timeVal env e)
inside env (After e) t
  = maybe False (<= t)  (timeVal env e)
inside env (Between e1 e2) t
  = fromMaybe False $ do t1 <- timeVal env e1
                         t2 <- timeVal env e2
                         return (t1 <= t && t < t2)
-}

-- classify a time value against an interval
timingVal :: Events -> Interval -> UTCTime -> Maybe Timing
timingVal env Always    t
  = return OnTime
timingVal env (Until e) t
  = do t' <- timeVal env e
       return (if t <= t' then OnTime else Overdue)
timingVal env (After e) t
  = do t' <- timeVal env e
       return (if t > t' then OnTime else Early)
timingVal env (Between e1 e2) t
  = do t1 <- timeVal env e1
       t2 <- timeVal env e2
       return (if t <= t1 then Early
               else if t <= t2 then OnTime
                    else Overdue)

-- * pretty-printing

showTimeExpr :: TimeZone -> Events -> TimeExpr -> Maybe String
showTimeExpr tz env e = (showLocalTime . utcToLocalTime tz) <$> timeVal env e 

-- showUTCTime :: UTCTime -> String
-- showUTCTime = formatTime defaultTimeLocale "%c" 

showLocalTime :: LocalTime -> String
showLocalTime = formatTime defaultTimeLocale "%c" 


-- format a time difference
formatNominalDiffTime :: NominalDiffTime -> String
formatNominalDiffTime secs 
  | secs>=0  = unwords $ 
               [show d ++ "d" | d>0] ++ 
               [show (h`rem`24) ++ "h" | h>0] ++
               [show (m`rem`60) ++ "m" | m>0] ++
               ["<1m" | secs<60]
  | otherwise = "--/--"
  where m = (floor (secs / 60)) :: Int
        h = (m `div` 60) 
        d = (h `div` 24)  
