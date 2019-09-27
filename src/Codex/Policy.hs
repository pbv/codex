{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}

{-
  Policies for classifying submissions as valid or invalid
-}

module Codex.Policy (
    TimeExpr,
    TimeEnv,
    Constr(..),
    Policy,
    -- early, late, lower, higher, maxAttempts,
    -- fromLocalTime,
    -- fromUTCTime,
    -- timeLeft, 
    -- parsePolicy,
    -- evalTimeExpr,
    -- timeLeft,
    parseTimeExpr,
    parsePolicy,
    makeTimeEnv,
    evalPolicy,
    showTime,
    formatNominalDiffTime
    ) where 

import           Data.Char
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time hiding (parseTime)

import           Text.ParserCombinators.ReadP

import           Control.Monad.Reader
import           Control.Monad.Except


-- | time expressions
data TimeExpr 
  = Event Name               -- ^ named event 
  | Local LocalTime          -- ^ local time constant
  | Add Diff TimeExpr        -- ^ add a time difference
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


-- | a submission policy is a list of constraints
-- i.e. a conjunction
type Policy t =  [Constr t] 

-- | policy constraints 
-- | parametrized by the type for time
data Constr t = Before t
              | After t
              | Attempts Int
              deriving (Eq, Show, Functor)

--
-- | parse policy constraints
--
parsePolicy :: String -> Either Text (Policy TimeExpr)
parsePolicy str
  = case readP_to_S (skipSpaces >> parseConstraintP) str of
       ((cs, ""):_) -> return cs
       ((_, txt):_) -> throwError ("can't parse constraint: " <>
                                    T.pack (show txt))
       _   -> throwError "can't parse constraint"


-- | parse a list of constraints
parseConstraintP :: ReadP [Constr TimeExpr]
parseConstraintP = do c <- parseBaseP; go [c]
                   <++ return  []
  where go cs = do symbol "and"; c<-parseBaseP; go (c:cs)
                <++ return (reverse cs)

-- | parse a single constraint
parseBaseP :: ReadP (Constr TimeExpr)
parseBaseP =
  do (symbol "before" <++ symbol "until"); Before <$> parseTimeP
  <++
  do symbol "after"; After <$> parseTimeP
  <++
  do symbol "attempts"; Attempts <$> integer



-- | parse time expressions; top-level wrapper funcion
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
-- a Monad for evaluating time expressions 
type TimeM = ExceptT Text (Reader TimeEnv)

-- | environments
data TimeEnv
  = TimeEnv
    { timeZone :: TimeZone
    , timeEvents :: Name -> Maybe TimeExpr
    }

makeTimeEnv :: TimeZone -> (Name -> Maybe TimeExpr) -> TimeEnv
makeTimeEnv = TimeEnv


-- | evaluate a time expression; wrapper
evalTimeExpr :: TimeExpr -> TimeM UTCTime
evalTimeExpr t = evalTime' [] t
  where
    -- worker function
    evalTime' :: [Name] -> TimeExpr -> TimeM UTCTime
    evalTime' deps (Event name)
      | name `elem` deps =
          throwError ("cyclic dependency in event: " <> T.pack (show name))
      | otherwise = do
          TimeEnv{..} <- ask
          case timeEvents name of
            Nothing -> throwError ("undefined event: " <> T.pack (show name))
            Just t -> evalTime' (name:deps) t
    evalTime' _ (Local t) = do
      TimeEnv{..} <- ask
      return (localTimeToUTC timeZone t)     
    evalTime' deps (Add d e)
      = addUTCTime (evalDiff d) <$> evalTime' deps e


evalDiff :: Diff -> NominalDiffTime
evalDiff (Diff value un) = fromIntegral value * toSeconds un
  where
    toSeconds Second = 1
    toSeconds Minute = 60
    toSeconds Hour   = 3600
    toSeconds Day    = 24*3600
    toSeconds Week   = 7*24*3600
  

--
-- | evaluate time expressions policy constraints
--

evalPolicy :: TimeEnv -> Policy TimeExpr -> Either Text (Policy UTCTime)
evalPolicy env cs = runReader (runExceptT (mapM evalConstr cs)) env

evalConstr :: Constr TimeExpr -> TimeM (Constr UTCTime)
evalConstr (Before te)  = Before <$> evalTimeExpr te
evalConstr (After te)   = After <$> evalTimeExpr te
evalConstr (Attempts n) = return (Attempts n)


-------------------------------------------------------------
-- pretty printing times
-------------------------------------------------------------

-- format an UTC time as local time
showTime :: TimeZone -> UTCTime -> Text
showTime tz = T.pack . formatTime defaultTimeLocale "%c"  . utcToLocalTime tz

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
