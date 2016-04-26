

module Interval where

import           Data.Char
import           Data.Maybe
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



-- parse intervals

readInterval :: ZonedTime -> String -> Maybe Interval
readInterval t txt
  = let tz = zonedTimeZone t
        loc= zonedTimeToLocalTime t
        parseI = do i<-parseInterval tz loc; skipSpaces; return i
    in
     case readP_to_S parseI txt of
       ((i, ""):_) -> Just i
       _ -> Nothing

parseInterval :: TimeZone -> LocalTime -> ReadP Interval
parseInterval tz now =
  do string "always"
     return Always
  <|>
  do string "until"
     skipSpaces
     Until <$> parseTimeExpr tz now
  <|>
  do string "after"
     skipSpaces
     After <$> parseTimeExpr tz now
  <|>
  do string "between"
     skipSpaces
     start <- parseTimeExpr tz now
     skipSpaces
     string "and"
     skipSpaces
     end <- parseTimeExpr tz now
     return (Between start end)
  
     
-- parse time expressions
parseTimeExpr :: TimeZone  -> LocalTime -> ReadP TimeExpr
parseTimeExpr tz now =
  do expr <- base
     skipSpaces
     (do diff <- parseDiff; return (Add diff expr)
      <++ return expr)
  where base = Absolute <$> parseUTCTime tz now
               <|>
               Event <$> parseName

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
  = do s <- parseSign
       skipSpaces
       ds <- many1' diff
       return (s * sum ds)
  where diff =  do
          x <- readS_to_P reads :: ReadP Int
          skipSpaces
          u <- parseUnit
          skipSpaces
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
many1' p = do x <- p; 
              (do xs <-many' p; return (x:xs)) <++ return [x]
              
many' p = many1' p <++ return []





-- * semantics
-- | a time environment
type TimeEnv = [(String, UTCTime)]

-- | time value 
timeVal :: TimeEnv -> TimeExpr -> Maybe UTCTime
timeVal env (Absolute t) = return t
timeVal env (Event n)    = lookup n env
timeVal env (Add d e)    = addUTCTime d <$> timeVal env e


-- | inside a time interval
inside :: TimeEnv -> Interval -> UTCTime -> Bool
inside env Always    t = True
inside env (Until e) t
  = maybe False (t <) (timeVal env e)
inside env (After e) t
  = maybe False (<= t)  (timeVal env e)
inside env (Between e1 e2) t
  = fromMaybe False $ do t1 <- timeVal env e1
                         t2 <- timeVal env e2
                         return (t1 <= t && t < t2)
    


