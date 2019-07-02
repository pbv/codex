
module Codex.Submission.Types where

import           Codex.Types
import           Codex.Tester.Result
import           Snap.Snaplet.SqliteSimple 
import           Data.Time.Clock (UTCTime)


-- | a row from the submssions table
data Submission = Submission {
  submitId     :: SubmitId,     -- submission DB id
  submitUser   :: UserLogin,    -- user login
  submitPath   :: FilePath,     -- exercise request path 
  submitTime   :: UTCTime,      -- submission time
  submitCode   :: Code,         -- submitted code
  submitResult :: Result        -- accepted/wrong answer/etc
  } 


submitLang :: Submission -> Language
submitLang = codeLang . submitCode


instance FromRow Submission where
  fromRow = do
    sid <- field
    uid <- field
    path <- field
    time <- field
    lang <- field
    text <- field 
    status <- field
    check <- field
    msg <- field
    let code = Code lang text
    let result = Result status check msg
    return (Submission sid uid path time code result)

