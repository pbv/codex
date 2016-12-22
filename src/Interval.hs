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
{-
data Interval = Always
              | Until !TimeExpr
              | After !TimeExpr
              | Between !TimeExpr !TimeExpr
              deriving (Eq, Show)
              -}

-- | an interval of `t's
data Interval t
    = Interval { lower :: Maybe t
               , higher :: Maybe t
               } deriving (Eq, Read, Show)



-- | a time expression
data TimeExpr
    = TimeEvent Text                     -- named time event
    | TimeConst LocalTime                -- literal (local time)
    | TimeAdd NominalDiffTime TimeExpr   -- add time difference
    deriving (Eq, Show)


-- | timing values
-- relation between a time and an Interval
data Timing = Early | Valid | Overdue
            deriving (Eq, Ord, Read, Show, Typeable)

{-
-- | get endpoints of an interval
from, until :: Interval -> Maybe TimeExpr
from (After e)     = Just e
from (Between e _) = Just e
from _             = Nothing

until (Until e)      = Just e
until (Between _ e)  = Just e
until _              = Nothing
-}


-- | parse an interval; yields an interval or an error message
parseInterval :: String -> Either Text (Interval TimeExpr)
parseInterval str
  = case readP_to_S parseIntervalP str of
       ((i, ""):_) -> Right i
       _ -> Left ("invalid interval \"" <> T.pack str <> "\"")


parseIntervalP :: ReadP (Interval TimeExpr)
parseIntervalP =
  do token "always"
     return (Interval Nothing Nothing)
  <++
  do token "until"
     t <- parseTimeExpr
     return (Interval Nothing (Just t))
  <++
  do token "after"
     t <- parseTimeExpr
     return (Interval (Just t) Nothing)
  <++
  do token "between"
     start <- parseTimeExpr
     token "and"
     end <- parseTimeExpr
     return (Interval (Just start) (Just end))


-- parse time expressions
parseTimeExpr :: ReadP TimeExpr
parseTimeExpr
  = do skipSpaces
       expr <- base
       (do {diff <- parseDiff; return (TimeAdd diff expr)}
        <++ return expr)
  where
    base = TimeConst <$> parseLocalTime <|>
           TimeEvent <$> parseEvent

-- parse local time strings
parseLocalTime ::  ReadP LocalTime
parseLocalTime =
  readS_to_P (readSTime True defaultTimeLocale "%H:%M %d/%m/%0Y")
  <++
  readS_to_P (readSTime True defaultTimeLocale "%d/%m/%0Y %H:%M")
  <++
  readS_to_P (readSTime True defaultTimeLocale "%d/%m/%0Y")


parseEvent :: ReadP Text
parseEvent = do
   char '\"'
   x <- satisfy isAlpha
   xs<- munch isAlphaNum
   char '\"'
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

parseSign :: ReadP NominalDiffTime
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


{-
parseUTCTime :: TimeZone -> LocalTime -> ReadP UTCTime
parseUTCTime tz now  = localTimeToUTC tz <$> parseLocalTime now
-}



-- semantics of time expressions
-- | an environment for events
type Events = [(Text, UTCTime)]

-- | semantics of a time expression
evalT :: TimeZone -> Events -> TimeExpr -> Either Text UTCTime
evalT tz events (TimeEvent ev)
  = case lookup ev events of
    Nothing -> Left ("undefined event: " <> "\"" <> ev <> "\"")
    Just t -> return t
evalT tz events (TimeConst t)
   = return (localTimeToUTC tz t)
evalT tz events (TimeAdd d e)
  = addUTCTime d <$> evalT tz events e


evalI :: TimeZone -> Events -> UTCTime
        -> Interval TimeExpr -> Either Text Timing
evalI tz events now (Interval (Just start) (Just end)) = do
  low <- evalT tz events start
  high <- evalT tz events end
  return (if now < low then Early
            else if now > high then Overdue
              else Valid)
evalI tz events now (Interval (Just start) Nothing) = do
  low <- evalT tz events start
  return (if now < low then Early else Valid)
evalI tz events now (Interval Nothing (Just end)) = do
    high <- evalT tz events end
    return (if now > high then Overdue else Valid)
evalI _ _ _ _ = return Valid


{-
evalI :: TimeZone -> UTCTime -> Events -> Interval TimeExpr
       -> Either Text Timing
evalI tz now events (Interval (Just l) (Just r))= do
   t1 <- evalT tz events l
   t2 <- evalT tz events r
   return (if now < t1 then Early
            else if now > t2 then Overdue
              else Valid)

evalI tz now events (Interval (Just l) Nothing) = do
  t1 <- evalT tz now events l
  return (if now < t1 then Early else Valid)
evalI tz now events (Interval Nothing (Just r)) = do
  t2 <- evalT tz now events r
  return (if now > t2 then Overdue else Valid)
evalI tz now events (Interval Nothing Nothing) =
  return Valid
-}


{-
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
  where m = floor (secs / 60) :: Int
        h = m `div` 60
        d = h `div` 24
