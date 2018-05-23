{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFunctor #-}

module Codex.Interval(
    Interval(..),
    TimeExpr,
    Events,
    Timing(..),
    rankTime,
    parseInterval,
    evalT,
    evalI,
    evalI',
    showTime,
    formatNominalDiffTime
    ) where

import           Data.Char
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Typeable
import           Data.Time
import           Control.Applicative

import           Text.ParserCombinators.ReadP


-- | an interval of `t's
data Interval t = Interval { lower :: !(Maybe t),
                             higher :: !(Maybe t) }
    deriving (Eq, Read, Show, Functor)


-- | abstract syntax for time expressions
data TimeExpr
    = TimeEvent Text                     -- named time event
    | TimeConst LocalTime                -- literal local time
    | TimeAdd NominalDiffTime TimeExpr   -- add time difference
    deriving (Eq, Show)


-- | timing values
-- relation between a time and an interval
data Timing = Early | Valid | Overdue
            deriving (Eq, Ord, Read, Show, Typeable)


rankTime :: Ord t => t -> Interval t -> Timing
rankTime t (Interval (Just low) (Just high))
   | t<low     = Early
   | t>high    = Overdue
   | otherwise = Valid
rankTime t (Interval (Just low) Nothing)
   = if t<low then Early else Valid
rankTime t (Interval Nothing (Just high))
   = if t<=high then Valid else Overdue
rankTime _ _  = Valid


-- | parse an interval
parseInterval :: String -> Maybe (Interval TimeExpr)
parseInterval str
  = case readP_to_S parseIntervalP str of
       ((i, ""):_) -> Just i
       _ -> Nothing -- ("invalid interval \"" <> T.pack str <> "\"")


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





-- semantics of time expressions
-- | an environment for events
type Events = Text -> Maybe UTCTime

-- | semantics of a time expression
evalT :: TimeZone -> Events -> TimeExpr -> Maybe UTCTime
evalT _ events (TimeEvent ev)
   = events ev -- lookup ev events
evalT tz _ (TimeConst t)
   = return (localTimeToUTC tz t)
evalT tz events (TimeAdd d e)
  = addUTCTime d <$> evalT tz events e


-- semantics for intervals
evalI :: TimeZone -> Events -> Interval TimeExpr -> Maybe (Interval UTCTime)
evalI tz events = evalI' (evalT tz events)

evalI' :: (t -> Maybe t') -> Interval t -> Maybe (Interval t')
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


{-
-- * pretty-printing
showTimeExpr :: TimeZone -> Events -> TimeExpr -> Maybe String
showTimeExpr tz env e
  = (showLocalTime . utcToLocalTime tz) <$> either (const Nothing) Just (evalT env e )


evalTimeExpr :: TimeZone -> Events -> TimeExpr -> Either Text Text
evalTimeExpr tz env e = showTime tz <$> evalT env e
-}


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
