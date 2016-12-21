{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}

module Interval where

import           Data.Char
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Monoid
import           Data.Typeable
import           Data.Time
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
data TimeExpr = Event !Text
              | Absolute !UTCTime
              | Add !NominalDiffTime !TimeExpr
          deriving (Eq, Show)


-- | timing values
data Timing = Valid
            | Early
            | Overdue
            deriving (Eq, Read, Show, Typeable)

-- | get endpoints of an interval
from, until :: Interval -> Maybe TimeExpr
from (After e)     = Just e
from (Between e _) = Just e
from _             = Nothing

until (Until e)      = Just e
until (Between _ e)  = Just e
until _              = Nothing

-- | parse an interval; yields an interval or an error message
readInterval :: ZonedTime -> String -> Either Text Interval
readInterval t txt
  = let tz = zonedTimeZone t
        loc= zonedTimeToLocalTime t
        parseI = parseInterval tz loc
    in
     case readP_to_S parseI txt of
       ((i, ""):_) -> Right i
       _ -> Left ("invalid interval \"" <> T.pack txt <> "\"")

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
parseTimeExpr :: TimeZone -> LocalTime -> ReadP TimeExpr
parseTimeExpr tz now
  = do expr <- base
       (do {diff <- parseDiff; return (Add diff expr)}
        <++ return expr)
  where
    base = Absolute <$> parseUTCTime tz now
           <|>
           Event <$> (skipSpaces >> parseEvent)


-- parse time strings
parseLocalTime :: LocalTime -> ReadP LocalTime
parseLocalTime now  =
  readS_to_P (readSTime True defaultTimeLocale "%H:%M %d/%m/%0Y")
   <++
   readS_to_P (readSTime True defaultTimeLocale "%d/%m/%0Y")
   <++
   (readS_to_P (readSTime True defaultTimeLocale "%H:%M") >>= \t ->
     return t { localDay = localDay now })


parseUTCTime :: TimeZone -> LocalTime -> ReadP UTCTime
parseUTCTime tz now  = localTimeToUTC tz <$> parseLocalTime now

parseEvent :: ReadP Text
parseEvent = do
  x <- satisfy isAlpha
  xs<- munch isAlphaNum
  return (T.pack (x:xs))


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
-- | an environment for events
type Events = [(Text, UTCTime)]

-- | semantics of a time expression
evalT :: Events -> TimeExpr -> Either Text UTCTime
evalT env (Absolute t)
  = return t

evalT env (Event n)
  = case lookup n env of
  Just t -> return t
  Nothing -> Left ("invalid event: " <> n)

evalT env (Add d e)
  = addUTCTime d <$> evalT env e


-- | semantics of a time interval
evalI :: Events -> Interval -> UTCTime -> Either Text Timing
evalI env Always t
  = return Valid
evalI env (Until e) t
  = do t' <- evalT env e
       return (if t <= t' then Valid else Overdue)
evalI env (After e) t
  = do t' <- evalT env e
       return (if t > t' then Valid else Early)
evalI env (Between e1 e2) t
  = do t1 <- evalT env e1
       t2 <- evalT env e2
       return (if t <= t1 then Early
               else if t <= t2 then Valid
                    else Overdue)

{-
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
-}

-- * pretty-printing
{-
showTimeExpr :: TimeZone -> Events -> TimeExpr -> Maybe String
showTimeExpr tz env e
  = (showLocalTime . utcToLocalTime tz) <$> either (const Nothing) Just (evalT env e )


evalTimeExpr :: TimeZone -> Events -> TimeExpr -> Either Text Text
evalTimeExpr tz env e = showTime tz <$> evalT env e
-}


showTime :: TimeZone -> UTCTime -> Text
showTime tz = T.pack . showLocalTime . utcToLocalTime tz

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
