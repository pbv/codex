{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Codex.Tester.Result where

import           Data.Typeable
import           Control.Exception
import           Data.Text(Text)
import qualified Data.Text as T

import           Database.SQLite.Simple.FromField
import           Database.SQLite.Simple.ToField

-- submission results
data Result = Result { resultClassify :: Classify
                     , resultMessage :: Text
                     }
              deriving (Eq, Read, Show, Typeable)

-- classification outcomes; ordered by severity
data Classify = Evaluating
              | MiscError
              | CompileError
              | RuntimeError
              | TimeLimitExceeded
              | MemoryLimitExceeded
              | PresentationError
              | WrongAnswer
              | Received
              | Accepted
              deriving (Eq, Ord, Read, Show, Typeable)

-- | convertions to/from SQL
instance ToField Classify where
  toField s = toField (show s)

instance FromField Classify where
  fromField f = do s <- fromField f
                   parse (reads s)
    where
      parse ((s,""):_) = return s
      parse _  = returnError ConversionFailed f "invalid Classify field"


instance Exception Result -- default instance


-- | result construtors
evaluating :: Result
evaluating = Result Evaluating ""

received, accepted, wrongAnswer, presentationError,
  compileError, runtimeError,
  timeLimitExceeded, memoryLimitExceeded, miscError :: Text -> Result

received = Result Received . trim maxLen
accepted = Result Accepted . trim maxLen
wrongAnswer = Result WrongAnswer . trim maxLen
compileError = Result CompileError . trim maxLen
runtimeError = Result RuntimeError . trim maxLen
timeLimitExceeded = Result TimeLimitExceeded . trim maxLen
memoryLimitExceeded = Result MemoryLimitExceeded . trim maxLen
miscError = Result MiscError . trim maxLen
presentationError = Result PresentationError . trim maxLen

maxLen :: Int
maxLen = 2000

-- | trim a text to a maximum length
trim :: Int -> Text -> Text
trim maxlen txt
  | T.length txt' <= maxlen = txt'
  | otherwise = T.append (T.take maxlen txt') "\n**Output too long (truncated)***\n"
  where txt' = T.strip txt
