{-# LANGUAGE OverloadedStrings, RecordWildCards, DeriveDataTypeable #-}
{-
   Evaluating, storing and fetching and submissions in Db
-}

module Submission where

import           System.FilePath
import           System.Directory
import           System.IO
import           System.Process
import           Data.Time.Clock
import           System.Exit (ExitCode)
import           Data.Maybe
import           Data.Typeable
import           Data.Map (Map)
import qualified Data.Map as Map

import           Data.Text(Text) 
import qualified Data.Text    as T
import qualified Data.Text.IO as T

import           Text.Regex

import           Control.Exception(bracket)

import           Control.Monad.State
import           Control.Applicative

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
    S.execute conn 
      "INSERT INTO submissions \
     \ (user_id, problem_id, time, code, status, report) \
     \ VALUES(?, ?, ?, ?, ?, ?)" (uid, pid, time, code, status, report)
    fmap (SID . fromIntegral) (S.lastInsertRowId conn)
  return (Submission sid uid pid time code status report)
  


-- | get a single user's submission 
getSubmission :: UID -> PID -> SID -> AppHandler Submission
getSubmission uid pid sid = do
  r <- query "SELECT * FROM submissions WHERE \
             \ id = ? AND user_id = ? AND problem_id = ?" (sid,uid,pid)
  case r of
    [s] -> return s
    _   -> notFound


-- | get all submissions for a user and problem
getSubmissions :: UID -> PID -> AppHandler [Submission]  
getSubmissions uid pid = 
  query "SELECT * FROM submissions \
       \ WHERE user_id = ? AND problem_id = ? ORDER BY id" (uid, pid)


-- | get user's submissions summary aggreated by problem IDs
-- result table rows : problem_id, #submissions, #accepted
getSubmissionsSummary :: UID -> AppHandler (Map PID (Int, Int))
getSubmissionsSummary uid  = do
  lst <- query "SELECT problem_id, COUNT(*), SUM(status='Accepted') \
              \ FROM submissions WHERE user_id = ? GROUP BY problem_id" (Only uid)
  return (Map.fromList [(pid,(count,accept)) | (pid,count,accept)<-lst])




-- | get the best submission for a problem for printouts:.
-- the last Accepted submission; or 
-- the last overall submission (if none was accepted)
-- Note: the query below assumes that the submissions ID key 
-- is monotonically increasing with time i.e. later submissions have higher IDs
getBestSubmission :: UID -> PID -> AppHandler (Maybe Submission)
getBestSubmission uid pid = 
  listToMaybe <$> 
  query "SELECT id,user_id,problem_id,time,code,status,report \
       \ FROM (SELECT *,status='Accepted' as accept \
             \ FROM submissions WHERE user_id = ? AND problem_id = ? \
             \ ORDER BY accept DESC, id DESC LIMIT 1)" (uid,pid) 

  

--
-- | post a new submission; top level function 
--
postSubmission :: UID -> 
                  Problem UTCTime -> 
                  Text -> 
                  AppHandler Submission
postSubmission uid prob code = 
  let pid = probID prob
      tstfile = "problems" </> show pid <.> "tst"
      tmpdir  = "tmp" </> show uid
  in do 
    now <- liftIO getCurrentTime
    -- create a temporary directory for this user (if missing)
    liftIO $ createDirectoryIfMissing True tmpdir 
    (exitCode, out, err) <- runSubmission tmpdir tstfile code 
    let (status, report) = makeReport now prob out err
    insertSubmission uid pid now code status report 


-- | run doctest file for a submissions
-- creates temp directory and file and cleanups afterwards
runSubmission :: FilePath -> FilePath -> Text -> AppHandler (ExitCode,String,String)
runSubmission tmpdir tstfile code = do 
  sf <- getSafeExec
  liftIO $ withTempFile tmpdir code (runDoctests sf tstfile)


-- | lower level I/O helper functions 
  
-- | make a python temporary file given a source code as Text
-- make sure file is cleaned up afterwards
withTempFile :: FilePath -> Text -> (FilePath -> IO a) -> IO a
withTempFile tmpdir txt = bracket create removeFile 
  where 
    create = do (file,handle) <- openTempFileWithDefaultPermissions tmpdir "tmp.py"
                T.hPutStr handle txt
                hClose handle
                return file




-- | run Python doctests inside a safeexec sandbox
runDoctests :: SafeExec -> FilePath -> FilePath -> IO (ExitCode, String, String)
runDoctests SafeExec{..} tstfile pyfile 
  = readProcessWithExitCode safeExec args ""
  where args = ["--cpu", show maxCpuTime,  
                "--clock", show maxClockTime,
                "--mem", show maxMemory, 
                "--exec", pythonExec, 
                "python/pytest.py", tstfile, pyfile]


  

-- | classify a submission and produce a text report
-- these rules are highly dependent on Python's doctest output 
makeReport :: UTCTime -> Problem UTCTime -> String -> String -> (Status, Text)
makeReport time prob out err 
  = (status, T.pack $ trim maxLen out ++ trim maxLen err)
  where 
    maxLen = 2000 -- max.length of stdout/stdout transcriptions
    status 
      | null out && match "OK" err   = if isOpen time prob 
                                       then Overdue
                                       else Accepted
      | match "Time Limit" err          = TimeLimitExceeded
      | match "Memory Limit" err        = MemoryLimitExceeded
      | match "Exception raised" out    = RuntimeError
      | match "SyntaxError" err         = CompileError
      | match "Failed" out              = WrongAnswer
      | otherwise                       = MiscError


-- | miscelaneous
-- | string wrapper over Text.Regex interface
match :: String -> String -> Bool
match re  = isJust . matchRegex (mkRegex re) 

-- | trim a string to a maximum length
trim :: Int -> String -> String
trim size str 
  = take (size - length msg) str ++ zipWith (\_ y->y) (drop size str) msg
  where msg = "...\n***Output too long (truncated)***"


  

