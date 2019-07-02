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
data Result
  = Result { resultStatus :: Status -- ^ see below
           , resultCheck :: Check  -- ^ valid/invalid timing, etc
           , resultReport :: Text -- ^ detailed report
           }
  deriving (Eq, Read, Show, Typeable)

-- | result check for valid submissions 
data Check = Valid
          | Invalid Text  -- ^ error message
          deriving (Eq, Read, Show, Typeable)

instance Semigroup Check where
  Invalid msg1 <> Invalid msg2 = Invalid (msg1 <> msg2)
  Valid <> b  = b
  a <> Valid  = a

instance Monoid Check where
  mempty = Valid
  

-- | classification statuses, in increasing severity (?)
data Status = Evaluating
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
instance ToField Status where
  toField = toField . show 

instance FromField Status where
  fromField f = fromField f >>= parse . reads
    where
      parse ((s,""):_) = return s
      parse _  = returnError ConversionFailed f "invalid Status field"

instance ToField Check where
  toField = toField . show


instance FromField Check where
  fromField f = fromField f >>= parse . reads 
    where
      parse ((s,""):_) = return s
      parse _  = returnError ConversionFailed f "invalid Check field"
  

instance Exception Result -- default instance


-- | result construtors
evaluating :: Result
evaluating = Result Evaluating Valid ""

received, accepted,
  wrongAnswer, presentationError, compileError, runtimeError,
  timeLimitExceeded, memoryLimitExceeded, miscError :: Text -> Result

received = Result Received Valid . trim maxLen
accepted = Result Accepted Valid . trim maxLen
wrongAnswer = Result WrongAnswer Valid . trim maxLen
compileError = Result CompileError Valid . trim maxLen
runtimeError = Result RuntimeError Valid . trim maxLen
timeLimitExceeded = Result TimeLimitExceeded Valid . trim maxLen
memoryLimitExceeded = Result MemoryLimitExceeded Valid . trim maxLen
miscError = Result MiscError Valid . trim maxLen
presentationError = Result PresentationError Valid . trim maxLen

maxLen :: Int
maxLen = 2000

-- | trim a text to a maximum length
trim :: Int -> Text -> Text
trim maxlen txt
  | T.length txt' <= maxlen = txt'
  | otherwise = T.append (T.take maxlen txt') "\n**Output too long (truncated)***\n"
  where txt' = T.strip txt
