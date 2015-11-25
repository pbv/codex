{-# LANGUAGE OverloadedStrings, RecordWildCards, DeriveDataTypeable #-}
{-
   Evaluating, storing and fetching and submissions in database
-}

module Submission where

import           System.FilePath
import           System.Directory
import           System.IO
-- import           System.Process
import           Data.Time.Clock
-- import           System.Exit (ExitCode)
import           Data.Maybe
import           Data.Typeable
import           Data.String

import qualified Data.ByteString.UTF8 as B
import           Data.Text(Text) 
import qualified Data.Text    as T
import qualified Data.Text.IO as T

-- import           Text.Regex

--import           Control.Exception(bracket)
import           Control.Monad.Trans (liftIO)
import           Control.Monad.State (gets)
import           Control.Applicative

import           Data.Aeson (encode)
import           Snap.Core
import           Snap.Snaplet.SqliteSimple
import qualified Database.SQLite.Simple as S
import           Database.SQLite.Simple.FromField 
import           Database.SQLite.Simple.ToField 

import           Application
import           Utils
import           Types
import           Language
import           Problem
import           Tester






-- | insert a new submission into the DB
insertSubmission :: ID User -> ID Problem -> UTCTime ->
                    Code Python -> Status -> Text -> Pythondo Submission
insertSubmission uid pid time code status report = do
  addr <- fmap (T.pack . B.toString) (getsRequest rqRemoteAddr)
  sid <- withSqlite $ \conn -> do
    S.execute conn 
      "INSERT INTO submissions \
     \ (user_id, problem_id, ip_addr, time, code, status, report) \
     \ VALUES(?, ?, ?, ?, ?, ?, ?)" (uid, pid, addr, time, code, status, report)
    fmap (fromString . show) (S.lastInsertRowId conn)
  return (Submission sid uid pid addr time code status report)
  


-- | get a single submission 
getSubmission :: ID User -> ID Problem -> ID Submission -> Pythondo Submission
getSubmission uid pid sid = do
  r <- query "SELECT * FROM submissions WHERE \
             \ id = ? AND user_id = ? AND problem_id = ?" (sid,uid,pid)
  case r of
    [s] -> return s
    _   -> notFound


-- | get all submissions for a user and problem
getSubmissions :: ID User -> ID Problem -> Pythondo [Submission]  
getSubmissions uid pid = 
  query "SELECT * FROM submissions \
       \ WHERE user_id = ? AND problem_id = ? ORDER BY id" (uid, pid)


-- | count the submissions for a problem
getSubmitCount :: Status -> ID User -> ID Problem -> Pythondo Int
getSubmitCount status uid pid = do
  r <- listToMaybe <$>
       query "SELECT COUNT(*) \
             \ FROM submissions WHERE status = ? \
             \ AND  user_id = ? AND problem_id = ?" (status,uid,pid)
  return $ maybe 0 fromOnly r
    

-- | count the total number of submissions for a problem
getTotalSubmissions :: ID User -> ID Problem -> Pythondo Int
getTotalSubmissions uid pid = do
    r <- listToMaybe <$>
         query "SELECT COUNT(*) \
               \ FROM submissions WHERE user_id = ? AND problem_id = ?" (uid,pid)
    return $ maybe 0 fromOnly r


-- | get the best submission for a problem for printouts:.
-- the last Accepted submission; or 
-- the last overall submission (if none was accepted)
-- Note: the query below assumes that the submissions ID key 
-- is monotonically increasing with time i.e. later submissions have higher IDs
getBestSubmission :: ID User -> ID Problem -> Pythondo (Maybe Submission)
getBestSubmission uid pid = 
  listToMaybe <$> 
  query "SELECT id,user_id,problem_id,ip_addr,time,code,status,report \
       \ FROM (SELECT *,status IN ('Accepted', 'Overdue') as accept \
             \ FROM submissions WHERE user_id = ? AND problem_id = ? \
             \ ORDER BY accept DESC, id DESC LIMIT 1)" (uid,pid) 



--
-- | post a new submission
--
postSubmission :: ID User -> Problem -> Code Python -> Pythondo Submission
postSubmission uid Problem{..} code = 
  let doctest = maybe "" id probSpec 
  in do 
    pyConf  <- gets pythonConf
    safeConf <- gets safeExecConf
    now <- liftIO getCurrentTime
    r <- liftIO (pythonTester pyConf safeConf code doctest)
    insertSubmission uid probID now code (resultStatus r) (resultMsg r)





-- | update Db table of problems
updateProblem :: Problem -> Pythondo ()
updateProblem Problem{..} =
  execute "INSERT OR UPDATE problems(problem_id, attrs, time_limit) VALUES (?, ?, ?)" (probID, encode probAttrs, probLimit)

