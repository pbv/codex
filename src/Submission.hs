{-# LANGUAGE OverloadedStrings, RecordWildCards, DeriveDataTypeable #-}
--
-- Storing and evaluating submissions 
--

module Submission where

import           System.FilePath
import           System.Directory
import           System.IO
import           System.Process
import           Data.Time.Clock
import           System.Exit (ExitCode)
import           Data.Maybe
import           Data.Typeable

import           Data.Text(Text) 
import qualified Data.Text    as T
import qualified Data.Text.IO as T

import           Text.Regex
import           Control.Exception

import           Control.Monad
import           Control.Monad.State
import           Control.Applicative

import           Snap.Snaplet
import           Snap.Snaplet.SqliteSimple
import qualified Database.SQLite.Simple as S
import           Database.SQLite.Simple.FromField 
import           Database.SQLite.Simple.ToField 

import           Application
import           Utils
import           Types
import           Problem


-- | single row in the submission DB
data Submission = Submission {
  submitID   :: SID,           -- unique id 
  submitUID  :: UID,
  submitPID  :: PID,
  submitTime :: UTCTime,       -- submission time  
  submitText :: Text,          -- submission text (program code)
  submitStatus :: Status,
  submitReport :: Text
  }


-- | result status of a submission
data Status = Accepted          -- passed all tests, in time
            | Overdue           -- passed all tests, late submission
            | WrongAnswer
            | CompileError
            | RuntimeError
            | TimeLimitExceeded
            | MemoryLimitExceeded
            | MiscError
              deriving (Eq, Ord, Show, Read, Typeable)

-- | convertion to/from SQL data
instance ToField Status where
  toField s = toField (show s)

instance FromField Status where
  fromField f = do s <- fromField f 
                   parse (reads s)
    where 
      parse ((s,""):_) = return s
      parse _  = returnError ConversionFailed f "couldn't parse status field" 


instance FromRow Submission where
  fromRow = Submission <$> field <*> field <*> field <*> field <*> field <*> field <*> field


-- | check if a submission is accepted
isAccepted :: Submission -> Bool
isAccepted Submission{..} = submitStatus==Accepted



-- | insert a new submission into the DB
insertSubmission ::  UID -> PID -> UTCTime -> Text -> Status -> Text 
                  -> AppHandler Submission
insertSubmission uid pid time code status report = do
  sid <- withSqlite $ \conn -> do
    S.execute conn "INSERT INTO submissions (user_id, problem_id, time, code, status, report) VALUES(?, ?, ?, ?, ?, ?)" (uid, pid, time, code, status, report)
    fmap (SID . fromIntegral) (S.lastInsertRowId conn)
  return (Submission sid uid pid time code status report)
  

-- | get all submissions for a user and problem
getSubmissions :: UID -> PID -> AppHandler [Submission]  
getSubmissions uid pid = 
  query "SELECT * FROM submissions WHERE user_id = ? AND problem_id = ? ORDER BY time" (uid, pid)

-- | get a single submission 
getSubmission :: UID ->  SID -> AppHandler Submission
getSubmission uid sid = do
  r <- query "SELECT * FROM submissions WHERE id = ? AND user_id = ?" (sid,uid)
  case r of
    [s] -> return s
    _   -> notFound


-- | get all the user submissions 
getAllSubmissions :: UID -> AppHandler [Submission]
getAllSubmissions uid = 
  query "SELECT * FROM submissions WHERE user_id = ? ORDER BY problem_id" (Only uid)



-- | post a new submission; top level function 
postSubmission :: UID -> Problem UTCTime -> UTCTime -> Text 
                  -> AppHandler Submission
postSubmission uid prob now code = do 
  sf <- getSafeExec
  (exitCode, stdout, stderr) <- liftIO $ runSubmission sf uid (probID prob) code 
  let (status, report) = makeReport now prob stdout stderr
  insertSubmission uid (probID prob) now code status report 


-- | lower level I/O helper functions 

-- | run doctest file for a submissions
-- creates temp directory and file and cleanups afterwards
runSubmission :: SafeExec -> UID -> PID -> Text -> IO (ExitCode,String,String)
runSubmission sf uid pid code = 
  let tmpdir = "tmp" </> show uid
      tstfile = "problems" </> show pid <.> "tst"
  in do
    createDirectoryIfMissing True tmpdir
    -- default permissions to allow safeexec reading
    (pyfile,handle) <- openTempFileWithDefaultPermissions tmpdir "tmp.py"
    T.hPutStr handle code
    hClose handle
    result <- runTests sf tstfile pyfile
    removeFile pyfile
    return result


-- | run python tests inside a safeexec sandbox
runTests :: SafeExec -> FilePath -> FilePath -> IO (ExitCode, String, String)
runTests SafeExec{..} tstfile pyfile 
  = readProcessWithExitCode safeExec args ""
  where args = ["--cpu", show maxCpuTime,  
                "--clock", show maxClockTime,
                "--mem", show maxMemory, 
                "--exec", pythonExec, 
                "python/pytest.py", tstfile, pyfile]


  

-- | classify a submission and produce a text report
-- these rules are highly dependent on Python's doctest output 
makeReport :: UTCTime -> Problem UTCTime -> String -> String -> (Status, Text)
makeReport time prob stdout stderr 
  = (status, T.pack $ trim maxLen stdout ++ trim maxLen stderr)
  where 
    maxLen = 2000 -- max.length of stdout/stdout transcriptions
    status 
      | null stdout && match "OK" stderr   = if isLate time prob 
                                             then Overdue
                                             else Accepted
      | match "Time Limit" stderr          = TimeLimitExceeded
      | match "Memory Limit" stderr        = MemoryLimitExceeded
      | match "Exception raised" stdout    = RuntimeError
      | match "SyntaxError" stderr         = CompileError
      | match "Failed" stdout              = WrongAnswer
      | otherwise                          = MiscError


-- | miscelaneous
-- | string wrapper over Text.Regex interface
match :: String -> String -> Bool
match re  = isJust . matchRegex (mkRegex re) 

-- | trim a string to a maximum length
trim :: Int -> String -> String
trim size str 
  = take (size - length msg) str ++ zipWith (\_ y->y) (drop size str) msg
  where msg = "...\n***Output too long (truncated)***"


  

