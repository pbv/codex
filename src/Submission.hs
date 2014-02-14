{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
{-
  Data types and methods for storing and evaluating submissions 
-}

module Submission where

import           Prelude hiding (catch)
import           System.FilePath
import           System.Directory
import           System.Directory.Tree
import           System.Process
import           Data.Time.Clock
import           Data.Time.Calendar
import           System.Time
import           System.Exit (ExitCode)
import           Data.Maybe
import           Data.Map(Map)
import qualified Data.Map as Map
import           Data.List (sort)
import           Data.Text(Text) 
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.ByteString.Lazy as LB       -- for XML serialization
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
  submitID   :: SID,     -- unique id (derived from filepath)
  submitText :: Text,    -- submission text (program code)
  submitTime :: UTCTime  -- submission time
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


-- check if a submission was accepted
accepted :: Report -> Bool
accepted r = reportStatus r == Accepted 


-- | convertion to/from XHTML
instance ToXHTML Report where
  toDocument Report{..} = htmlDocument [status, stdout, stderr]
    where status = X.Element "status" [] 
                    [X.TextNode (T.pack $ show reportStatus)]
          stdout = X.Element "stdout" [] [X.TextNode reportStdout]
          stderr = X.Element "stderr" [] [X.TextNode reportStderr]
          


-- | XML reader for a report
reportReader :: XMLReader Report
reportReader = do 
  status <- tagged "status" statusReader
  stdout <- tagged "stdout" allText
  stderr <- tagged "stderr" allText
  return (Report status stdout stderr)
                   

statusReader :: XMLReader Status
statusReader = do t <- allText
                  case reads (T.unpack t) of
                    [] -> fail "statusReader: no parse"
                    ((s, _):_) -> return s


-- doctest file for a problem
doctestFile ::  PID -> FilePath
doctestFile pid = "problems" </> show pid <.> "tst"

-- submission directory associated for a user and problem
submissionDir :: UID -> PID -> FilePath
submissionDir uid pid = "submissions" </> show uid </> show pid



-- | Create a new submission 
-- this should be locked using an MVar to ensure thread safety
newSubmitID :: UID -> PID -> Text -> IO SID
newSubmitID uid pid text = 
  let dir = submissionDir uid pid
  in do
    sids <- allSubmitIDs uid pid 
    let sid = head [SID n | n <- [1..], SID n`notElem`sids]
    T.writeFile (dir </> show sid <.> "py") text
    return sid


-- | fetch all submission IDs for a user and problem
-- in ascending order of submission time
allSubmitIDs :: UID -> PID -> IO [SID]
allSubmitIDs uid pid 
  = let dir = submissionDir uid pid
    in do 
      createDirectoryIfMissing True dir
      files <- getDirectoryContents dir
      return (sort $ ids files)
  where ids files
          = [sid | f<-files, snd (splitExtension f)==".py", 
             (sid, _)<-reads (dropExtension f) ]


-- | get the final submission report:
-- the last accepted submission 
-- or the last submission (if none was accepted)

getFinalReport ::  UID -> Problem UTCTime -> AppHandler (Maybe (Submission,Report))
getFinalReport uid prob 
  = do lst <- getSubmitReports uid prob 
       let lst' = reverse lst
       return $ listToMaybe $ dropWhile (not.accepted.snd) lst' ++ lst'


{-
finalSubmitReport :: [(Submission,Report)] -> Maybe (Submission,Report)
finalSubmitReport lst = listToMaybe (dropWhile (not.accepted.snd) lst' ++ lst')
  where lst' = reverse lst   -- process in reverse submission order
-}

-- | get all submissions paired with reports in chronological order
getSubmitReports ::  UID -> Problem UTCTime -> AppHandler [(Submission,Report)]
getSubmitReports uid prob
  = do sf <- getSafeExec
       liftIO $ do sids <- allSubmitIDs uid (probID prob)
                   mapM (getSubmitReport' sf uid prob) sids
            


getSubmitReport :: UID -> Problem UTCTime -> SID 
                   -> AppHandler (Submission,Report)
getSubmitReport uid prob sid 
  = do sf<-getSafeExec; liftIO (getSubmitReport' sf uid prob sid)

-- | get the report for a submission
-- uses the report file if cached; runs python tests if not
getSubmitReport' :: SafeExec -> UID -> Problem UTCTime -> SID 
                   -> IO (Submission,Report)
getSubmitReport' sf uid prob sid =
  let pid = probID prob
      dir = submissionDir uid pid
      tst = doctestFile pid
      py = dir </> show sid <.> "py"
      out = dir </> show sid <.> "out"
  in do 
    txt <- T.readFile py
    t <- getUTCModificationTime py
    report <- catch (readFromHTMLFile out reportReader) 
      (\e -> do -- ignore exception and generate report
          let _ = e :: IOException
          (_, stdout, stderr) <- runTests sf tst py
          let r = makeReport (acceptable t prob) stdout stderr
          LB.writeFile out (toLazyByteString $ X.render $ toDocument r)
          return r)
    return (Submission {submitID=sid, submitText=txt, submitTime=t}, report)
            

-- | an hugly hack to work around old System.Directory 
getUTCModificationTime :: FilePath -> IO UTCTime 
getUTCModificationTime fp = 
    getModificationTime fp >>= toCalendarTime >>= \t -> return (ctToUTC t)

ctToUTC :: CalendarTime -> UTCTime
ctToUTC ct = UTCTime day diff 
  where day = fromGregorian (fromIntegral $ ctYear ct) (1+fromEnum (ctMonth ct)) (ctDay ct)
        diff = secondsToDiffTime (fromIntegral $ ctHour ct*3600 + ctMin ct*60 + ctSec ct)


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
  = take size str ++ 
    zipWith (\x y->y) (drop size str) "...\n***Output too long (truncated)***"


-------------------------------------------------------------------------------
-- get the status of all user and problems 
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
                base= takeBaseName file



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
  
  