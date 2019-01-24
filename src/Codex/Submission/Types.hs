
module Codex.Submission.Types where

import           Codex.Types
import           Codex.Time (Timing)
import           Codex.Tester.Result
import           Database.SQLite.Simple.FromField
import           Database.SQLite.Simple.ToField
import           Snap.Snaplet.SqliteSimple 
import           Data.Time.Clock


-- | a row in the submssion table
data Submission = Submission {
  submitId     :: SubmitId,     -- submission DB id
  submitUser   :: UserLogin,    -- user login
  submitPath   :: FilePath,     -- exercise request path 
  submitTime   :: UTCTime,      -- submission time
  submitCode   :: Code,         -- submitted code
  submitResult :: Result,       -- accepted/wrong answer/etc
  submitTiming :: Timing        -- timing (early, valid, overdue)
  } 


submitLang :: Submission -> Language
submitLang = codeLang . submitCode

instance ToField Timing where
  toField s = toField (show s)

instance FromField Timing where
  fromField f = do s <- fromField f
                   parse (reads s)
    where
      parse ((s,""):_) = return s
      parse _  = returnError ConversionFailed f "invalid Timing field"

instance FromRow Submission where
  fromRow = do
    sid <- field
    uid <- field
    path <- field
    time <- field
    lang <- field
    text <- field 
    classf <- field
    msg <- field
    tv <- field
    let code = Code lang text
    let result = Result classf msg
    return (Submission sid uid path time code result tv)

