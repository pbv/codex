{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Codex.Tester.Result
  (Result(..), Status(..),
   evaluating, received, accepted, presentationError,
   compileError, wrongAnswer, runtimeError,
   timeLimitExceeded, memoryLimitExceeded, miscError
  ) where

import           Data.Typeable
-- import           Control.Exception
import           Data.Text(Text)
import qualified Data.Text as T

import           Database.SQLite.Simple.FromField
import           Database.SQLite.Simple.ToField

-- submission results
data Result
  = Result { resultStatus :: Status   -- ^ see below
           , resultReport :: Text     -- ^ detailed report
           }
  deriving (Eq, Read, Show, Typeable)


-- | classification status, in decreasing severity 
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
  

-- instance Exception Result -- default instance

-- combine two results, yielding the "worse" status and joint reports
instance Semigroup Result where
  Result s1 r1 <> Result s2 r2
    = Result (s1 `min` s2)  (r1 <> r2)
        
instance Monoid Result where
  mempty = Result Accepted ""


-- | result construtors
evaluating :: Result
evaluating = Result Evaluating ""

received, accepted,
  wrongAnswer, presentationError, compileError, runtimeError,
  timeLimitExceeded, memoryLimitExceeded, miscError :: Text -> Result

received = Result Received . trim maxLen
accepted = Result Accepted  . trim maxLen
wrongAnswer = Result WrongAnswer . trim maxLen
compileError = Result CompileError . trim maxLen
runtimeError = Result RuntimeError . trim maxLen
timeLimitExceeded = Result TimeLimitExceeded . trim maxLen
memoryLimitExceeded = Result MemoryLimitExceeded . trim maxLen
miscError = Result MiscError  . trim maxLen
presentationError = Result PresentationError . trim maxLen

maxLen :: Int
maxLen = 2000

-- | trim a text to a maximum length
trim :: Int -> Text -> Text
trim maxlen txt
  | T.length txt <= maxlen = txt
  | otherwise = T.append (T.take maxlen txt) "\n**Output too long (truncated)***\n"

