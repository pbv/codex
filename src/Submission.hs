{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-
  Data types and methods for storing and evaluating submissions 
-}

module Submission where

-- import           Prelude hiding (catch)
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
-- import qualified Data.ByteString.Lazy as LB -- for XML serialization

import           Text.Regex
import           Control.Exception
-- import           Blaze.ByteString.Builder

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



{-
-- | post a new submission 
-- use mutexIO to ensure thread safety
postSubmission :: UID -> PID -> Text -> AppHandler SID
postSubmission uid pid text = 
  let dir = submissionDir uid pid 
  in mutexIO $ do
    sids <- listSubmissions' uid pid 
    let sid = head [SID n | n <- [1..], SID n`notElem`sids]
    T.writeFile (dir </> show sid <.> "py") text
    return sid
-}

{-
-- | list all submission IDs in ascending order for a user and problem
listSubmissions ::  UID -> PID -> AppHandler [SID]
listSubmissions uid pid = liftIO (listSubmissions' uid pid)
    
listSubmissions' :: UID -> PID -> IO [SID]
listSubmissions' uid pid 
  = do createDirectoryIfMissing True dir
       files <- getDirectoryContents dir
       return (sort $ ids files)
  where dir = submissionDir uid pid
        ids files = [sid | f<-files, 
                     snd (splitExtension f)==".py", 
                     (sid, _)<-reads (dropExtension f)]
-}        

{-
-- | get the final submission report:
-- the last accepted submission 
-- or the last submission (if none was accepted)
getFinalReport ::  UID -> Problem UTCTime -> AppHandler (Maybe (Submission,Report))
getFinalReport uid prob 
  = do lst <- getSubmitReports uid prob 
       let lst' = reverse lst
       return $ listToMaybe $ dropWhile (not.accepted.snd) lst' ++ lst'
-}

{-
finalSubmitReport :: [(Submission,Report)] -> Maybe (Submission,Report)
finalSubmitReport lst = listToMaybe (dropWhile (not.accepted.snd) lst' ++ lst')
  where lst' = reverse lst   -- process in reverse submission order
-}

{-
-- | get all submissions with reports in chronological order
getReports ::  UID -> Problem UTCTime -> AppHandler [Submission]
getReports uid prob
  = do sf <- getSafeExec
       liftIO $ do sids <- getSubmissions uid (probID prob)
                   mapM (getReport' sf uid prob) sids
-}            

{-
getReports :: UID -> Problem UTCTime -> AppHandler [Submission]
getReports uid prob 
  = listSubmissions uid (probID prob) >>= mapM (getReport uid prob) 

-- | Get a submission together with report
getReport :: UID -> Problem UTCTime -> SID -> AppHandler Submission
getReport uid prob sid = do sf<-getSafeExec; liftIO (getReport' sf)
  where
    pid = probID prob
    dir = submissionDir uid pid
    tst = doctestFile pid
    py = dir </> show sid <.> "py"
    out = dir </> show sid <.> "out"
    -- try reading the report file; run python tests if not available
    getReport' sf = do
      txt <- T.readFile py
      time <- getModificationTime py
      report <- catch (readFromHTMLFile out reportReader) (\e -> do
        -- ignore exception and generate report
        let _ = e :: IOException
        (_, stdout, stderr) <- runTests sf tst py
        let r = makeReport time prob stdout stderr
        LB.writeFile out (toLazyByteString $ X.render $ reportToDoc r)
        return r)
      return Submission {submitID=sid, 
                         submitText=txt, 
                         submitTime=time, 
                         submitReport=Just report}
            
-}

-- | post a new submission; top level function 
postSubmission :: UID -> Problem UTCTime -> UTCTime -> Text -> AppHandler Submission
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


{-
-- | quick and dirty hack around return time of old 
--   System.Directory.getModificationTime
getUTCModificationTime :: FilePath -> IO UTCTime 
getUTCModificationTime fp 
  = getModificationTime fp >>= toCalendarTime >>= (\t -> return (ctToUTC t))

ctToUTC :: CalendarTime -> UTCTime
ctToUTC ct = UTCTime day diff 
  where day = fromGregorian (fromIntegral $ ctYear ct) (1+fromEnum (ctMonth ct)) (ctDay ct)
        diff = secondsToDiffTime (fromIntegral $ ctHour ct*3600 + ctMin ct*60 + ctSec ct)
-}

  

-- | classify a submission and produce a text report
-- WARNING: these rules are highly dependent on Python's doctest output 
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

---------------------------------------------------------------------------
  
{-        
-- doctest file for a problem
doctestFile ::  PID -> FilePath
doctestFile pid = "problems" </> show pid <.> "tst"

-- submission directory associated for a user and problem
submissionDir :: UID -> PID -> FilePath
submissionDir uid pid = "submissions" </> show uid </> show pid
-}

