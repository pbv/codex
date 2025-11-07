{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeApplications #-}

module Codex.Tester.Result
  (Result(..), Status(..), Report(..), Visibility(..),
   evaluating, received, accepted, presentationError,
   compileError, wrongAnswer, runtimeError,
   timeLimitExceeded, memoryLimitExceeded, miscError,
   tagWith, hidePrivate
  ) where

import qualified Data.Text as T

import           Database.SQLite.Simple.FromField
import           Database.SQLite.Simple.ToField

import           Text.Pandoc.Definition
import           Text.Pandoc.Builder
import           Text.Pandoc.Walk

-- submission results
data Result
  = Result { resultStatus :: Status   -- ^ possible status; see below
           , resultReport :: Report   -- ^ detailed report
           }
  deriving (Eq, Read, Show)

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
            deriving (Eq, Ord, Read, Show) 

-- | convertions to/from SQL
instance ToField Status where
  toField = toField . show

instance FromField Status where
  fromField f = fromField @String f >>= parse . reads
    where
      parse ((s,""):_) = return s
      parse _  = returnError ConversionFailed f "invalid Status field"

-- newtype for submission reports;
-- wrapper over Pandoc's sequence of blocks
newtype Report = Report { getBlocks :: Blocks }
  deriving stock (Eq, Show, Read)
  deriving (Semigroup, Monoid) via Blocks

instance ToField Report where
  toField (Report blocks)
    = toField (show $ doc blocks)

instance FromField Report where
  fromField f = fromField @String f >>= parse . reads
    where
      parse ((Pandoc _ bs,""):_) = return (Report $ fromList bs)
      parse _ = returnError ConversionFailed f "invalid Report field"

-- combine two results, yielding the "worse" status and joining the reports
instance Semigroup Result where
  Result s1 r1 <> Result s2 r2
    = Result (s1 `min` s2)  (r1 <> r2)

instance Monoid Result where
  mempty = Result Accepted mempty

-- | result construtors
evaluating :: Result
evaluating = Result Evaluating mempty

received, accepted,
  wrongAnswer, presentationError, compileError, runtimeError,
  timeLimitExceeded, memoryLimitExceeded, miscError :: Blocks -> Result

received = Result Received . Report
accepted = Result Accepted . Report
wrongAnswer = Result WrongAnswer . Report
compileError = Result CompileError . Report
runtimeError = Result RuntimeError . Report
timeLimitExceeded = Result TimeLimitExceeded . Report 
memoryLimitExceeded = Result MemoryLimitExceeded . Report
miscError = Result MiscError . Report
presentationError = Result PresentationError . Report  

data Visibility = Public | Private
     deriving (Eq, Read, Show)

onReport :: (Blocks -> Blocks) -> Result -> Result
onReport f r = r { resultReport = Report $ f $ getBlocks $ resultReport r }

tagWith :: Visibility -> Result -> Result
tagWith _ r | r == mempty = r
tagWith Public  r 
    = onReport (header 2 "Public tests" <>) r
tagWith Private r 
    = onReport (\bs -> header 2 "Private tests" <> 
                       divWith ("",["private"], []) bs) r

hidePrivate :: Result -> Result
hidePrivate r = onReport (walk hide) r
  where
    hide :: Block -> Block
    hide (Div (_,classes,_) _)
      | "private"`elem`classes 
           = Plain [Strong [Str $ T.pack $ show $ resultStatus r],
                    Space,
                    Emph [Str "(details hidden.)"]]
    hide block = block

