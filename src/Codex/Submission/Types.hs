
module Codex.Submission.Types where

import           Codex.Types
import           Codex.Tester.Result
import           Snap.Snaplet.SqliteSimple 
import           Data.Time.Clock (UTCTime)

import           Database.SQLite.Simple.FromField
import           Database.SQLite.Simple.ToField


-- | a row from the submssions table
data Submission = Submission {
  submitId     :: SubmitId,     -- submission DB id
  submitUser   :: UserLogin,    -- user login
  submitPath   :: FilePath,     -- exercise request path 
  submitTime   :: UTCTime,      -- submission time
  submitCode   :: Code,         -- submitted code
  submitResult :: Result,       -- accepted/wrong answer/etc
  submitCheck  :: Validity       -- w.r.t. submission policy
  } 


-- semantics
-- | result of policy check for submissions 
data Validity = Valid         -- ^ OK
              | Invalid Text  -- ^ rejection message
              deriving (Eq, Read, Show) --, Typeable)


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
    let result = Result status  msg
    return (Submission sid uid path time code result check)


invalid :: Text -> Validity
invalid = Invalid


instance ToField Validity where
  toField = toField . show

instance FromField Validity where
  fromField f = fromField f >>= parse . reads 
    where
      parse ((s,""):_) = return s
      parse _  = returnError ConversionFailed f "invalid Validity field"
