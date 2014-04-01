{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
{-
  Data types and methods for storing and evaluating submissions 
-}

module Submission 
       ( Submission(..),  
         Report(..),
         Status(..),
         isAccepted,        -- * check if submission is accepted
         listSubmissions,  -- * all submissions IDs for a problem
         postSubmission,   -- * write a new submissions
         getReport,    -- * fetch one submssion
         getReports   -- * fetch all submissions
       ) where

-- import           Prelude hiding (catch)
import           System.FilePath
import           System.Directory
import           System.Directory.Tree
import           System.Process
import           Data.Time.Clock
-- import           Data.Time.Calendar
-- import           System.Time
import           System.Exit (ExitCode)
import           Data.Maybe
--import           Data.Map(Map)
--import qualified Data.Map as Map
import           Data.List (sort)
import           Data.Text(Text) 
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.ByteString.Lazy as LB -- for XML serialization
import           Text.XmlHtml(Document)
import qualified Text.XmlHtml as X

import           Text.Regex
import           Control.Exception
import           XHTML
import           Blaze.ByteString.Builder

import           Control.Monad.State

import           Application
import           Utils
import           Types
import           Problem

-- | submission to a programming problem
data Submission = Submission {
  submitID   :: SID,           -- unique id (derived from filepath)
  submitText :: Text,          -- submission text (program code)
  submitTime :: UTCTime,       -- submission time
  submitReport :: Maybe Report -- optional report 
  } deriving Show

-- | submission report
data Report = Report { 
  reportStatus :: Status,  -- status of this submission (one of the below)
  reportStdout :: Text,    -- stdout/stderr transcriptions
  reportStderr :: Text
  } deriving Show

-- | result status of a submission
data Status = Accepted          -- passed all tests, in time
            | Overdue           -- passed all tests, late submission
            | WrongAnswer
            | CompileError
            | RuntimeError
            | TimeLimitExceeded
            | MemoryLimitExceeded
            | MiscError
              deriving (Eq, Ord, Show, Read)

-- check if a submission is accepted
isAccepted :: Submission -> Bool
isAccepted Submission{..} 
  = maybe False ((==Accepted).reportStatus) submitReport


{-
-- | convertion to/from XHTML
instance ToXHTML Report where
  toDocument Report{..} = htmlDocument [status, stdout, stderr]
    where status = X.Element "status" [] 
                    [X.TextNode (T.pack $ show reportStatus)]
          stdout = X.Element "stdout" [] [X.TextNode reportStdout]
          stderr = X.Element "stderr" [] [X.TextNode reportStderr]
          
-}



-- | Post a new submission 
-- use mutexIO to ensure thread safety
postSubmission :: UID -> PID -> Text -> AppHandler SID
postSubmission uid pid text = 
  let dir = submissionDir uid pid 
  in mutexIO $ do
    sids <- listSubmissions' uid pid 
    let sid = head [SID n | n <- [1..], SID n`notElem`sids]
    T.writeFile (dir </> show sid <.> "py") text
    return sid



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
      t <- getModificationTime py
      report <- catch (readFromHTMLFile out reportReader) (\e -> do
        -- ignore exception and generate report
        let _ = e :: IOException
        (_, stdout, stderr) <- runTests sf tst py
        let r = makeReport (isAcceptable t prob) stdout stderr
        LB.writeFile out (toLazyByteString $ X.render $ reportToDoc r)
        return r)
      return Submission {submitID=sid, 
                         submitText=txt, 
                         submitTime=t, 
                         submitReport=Just report}
            
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




-- | run python tests inside a safeexec sandbox
runTests :: SafeExec -> FilePath -> FilePath -> IO (ExitCode, String, String)
runTests SafeExec{..} tstfile pyfile 
  = readProcessWithExitCode safeExec args ""
  where args = ["--cpu", show maxCpuTime,  
                "--clock", show maxClockTime,
                "--mem", show maxMemory, 
                "--exec", pythonExec, "python/pytest.py", tstfile, pyfile]

  
-- evaluate a submission 
-- first argument: True if submission is within deadline, False otherwise
makeReport :: Bool -> String -> String -> Report
makeReport ontime stdout stderr 
  = Report { reportStatus = status
           , reportStdout = T.pack $ trim 2000 stdout
           , reportStderr = T.pack $ trim 2000 stderr 
           }
  where status 
          | null stdout && match "OK" stderr   = if ontime then Accepted
                                                 else Overdue
          | match "Time Limit" stderr          = TimeLimitExceeded
          | match "Memory Limit" stderr        = MemoryLimitExceeded
          | match "Exception raised" stdout    = RuntimeError
          | match "SyntaxError" stderr         = CompileError
          | match "Failed" stdout              = WrongAnswer
          | otherwise                          = MiscError


-- miscelaneous
-- string wrapper over Text.Regex interface
match :: String -> String -> Bool
match re  = isJust . matchRegex (mkRegex re) 

-- trim a string to a maximum length
trim :: Int -> String -> String
trim size str 
  = take (size - length msg) str ++ zipWith (\_ y->y) (drop size str) msg
  where msg = "...\n***Output too long (truncated)***"

---------------------------------------------------------------------------
  
-- internal stuff
-- convertion to/from XML

-- | XML reader for a report
reportReader :: XMLReader Report
reportReader = do 
  status <- statusReader
  stdout <- element "stdout" 
  stderr <- element "stderr" 
  return (Report status (X.nodeText stdout) (X.nodeText stderr))
  
statusReader :: XMLReader Status
statusReader = do n <- element "status" 
                  case reads (T.unpack $ X.nodeText n) of
                    [] -> fail "statusReader: no parse"
                    ((s, _):_) -> return s

-- convert a report into a document
reportToDoc :: Report -> Document
reportToDoc Report{..} = htmlDocument [status, stdout, stderr]
    where status = X.Element "status" [] 
                    [X.TextNode (T.pack $ show reportStatus)]
          stdout = X.Element "stdout" [] [X.TextNode reportStdout]
          stderr = X.Element "stderr" [] [X.TextNode reportStderr]
          

-- doctest file for a problem
doctestFile ::  PID -> FilePath
doctestFile pid = "problems" </> show pid <.> "tst"

-- submission directory associated for a user and problem
submissionDir :: UID -> PID -> FilePath
submissionDir uid pid = "submissions" </> show uid </> show pid


---------------------------------------------------------------
-- Submissions Queries
---------------------------------------------------------------
-- | a row from the submission database
data Row = Row { rowUID :: UID, 
                 rowPID :: PID,
                 rowSID :: SID,
                 rowReport :: Report
               } deriving Show
           
type Table = [Row] -- a table is a list of rows

-- | query producing an `a'
type Query a = Row -> a

(|||) :: Query Bool -> Query Bool -> Query Bool
q1 ||| q2 = \row -> q1 row || q2 row

(&&&) :: Query Bool -> Query Bool -> Query Bool
q1 &&& q2 = \row -> q1 row && q2 row


rowStatus :: Query Status
rowStatus =  reportStatus . rowReport

accepted :: Query Bool
accepted r = rowStatus r == Accepted

overdue :: Query Bool
overdue r = rowStatus r == Overdue

wrongAnswer :: Query Bool
wrongAnswer r = rowStatus r == WrongAnswer

runtimeError :: Query Bool
runtimeError r = rowStatus r == RuntimeError

miscError :: Query Bool
miscError r = rowStatus r == MiscError


select :: Query Bool -> [Row] -> [Row]
select = filter

project :: Query a -> [Row] -> [a]
project = map

-------------------------------------------------------------------------------
-- read the submission database lazily from disk
-------------------------------------------------------------------------------
readDB :: IO [Row]
readDB = do top <- readDirectoryWithL readf "submissions"
            return (mkRows $ filterDir (not.failed) $ dirTree top)
  where readf file 
          | ext == ".out" = readFromHTMLFile file reportReader
          | otherwise = ioError $ userError "ignored file"
          where ext = takeExtension file


mkRows :: DirTree Report -> [Row]
mkRows dir = [Row uid pid sid rep 
             | dir' <- contents dir,
               let uid = read (name dir'),
               dir'' <- contents dir',
               let pid = read (name dir''),
               File fp rep <- contents dir'',
               let sid = read (takeBaseName fp)
             ]

{-
getAllStatus :: IO (Map UID (Map PID [Status]))
getAllStatus =  fmap reportSummary readAllStatus
  
-- read all report status into a tree structure
readAllStatus :: IO (DirTree Status)
readAllStatus = do top <- readDirectoryWithL readf "submissions"
                   return (filterDir (not.failed) $ dirTree top)
  where readf file 
          | ext == ".out" = do r <- readFromHTMLFile file reportReader
                               return (reportStatus r)
          | otherwise = ioError $ userError "not a report file"
          where ext = takeExtension file

-- summary of all submission reports:
reportSummary :: DirTree Status -> Map UID (Map PID [Status])
reportSummary dir = 
  Map.fromList [ (uid, reportUser dir')
               | dir'<-contents dir, let uid = read (name dir')]
    
reportUser :: DirTree Status -> Map PID [Status]
reportUser dir =
  Map.fromList [(pid, status) 
               | Dir name dirs'<-contents dir, 
                 let pid = read name, 
                 let status = [stat | File _ stat <- dirs']]
  
-}
        

