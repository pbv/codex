{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-
   Evaluating, storing and fetching and submissions in database
-}

module Submission
       ( Submission(..)
       , Result(..)
       , Qualifier(..)
       , newSubmission
       , getSubmission
       , getSubmissions
       , getWorksheetSubmissions
--       , countSubmissions
--       , countSubmissions'
       ) where

import           System.FilePath
import           System.Directory
import           System.IO

import           Data.Time.Clock

import           Data.Maybe
import           Data.Typeable
import           Data.String

import qualified Data.ByteString.UTF8 as B
import           Data.Text(Text) 
import qualified Data.Text    as T
import qualified Data.Text.IO as T

import           Control.Monad.Trans (liftIO)
import           Control.Monad.State (gets)
import           Control.Applicative

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



-- | a row in the submssion table
data Submission = Submission {
  submitID   :: SubmitID,
  submitUID  :: UserID,
  submitPID  :: ProblemID,           -- problem id
  -- submitIPAddr :: Text,        -- client IP address
  submitTime :: UTCTime,       -- submit time  
  submitCode :: Code,   -- program code
  submitQualifier :: Qualifier,  -- in time/overdue
  submitResult :: Result,      -- accepted/wrong answer/etc
  submitMsg :: Text            -- detailed message
  }


-- | in-time or overdue?
data Qualifier = OK | Overdue deriving (Eq, Show, Read, Typeable)


-- | convertion to/from SQL
instance ToField Result where
  toField s = toField (show s)

instance FromField Result where
  fromField f = do s <- fromField f 
                   parse (reads s)
    where 
      parse ((s,""):_) = return s
      parse _  = returnError ConversionFailed f "invalid Result field"

instance ToField Qualifier where
  toField s = toField (show s)

instance FromField Qualifier where
  fromField f = do s <- fromField f
                   parse (reads s)
    where
      parse ((s,""):_) = return s
      parse _ = returnError ConversionFailed f "invalid Qualifier field"


instance FromRow Submission where
  fromRow = (Submission <$>
             field <*> field <*> field <*> field <*>
             field <*> field <*> field <*> field)


-- | evaluate a new submission
newSubmission :: UserID -> Problem -> Code -> AppHandler Submission
newSubmission uid Problem{..} code = do 
    now <- liftIO getCurrentTime
    (result, msg) <- probTester code
    let qualifier = if now `before` probLimit then OK else Overdue
    insertSubmission uid probID now code qualifier result msg

before :: Ord t => t -> Maybe t -> Bool
before now limit = maybe True (now<=) limit
          


-- | insert a new submission into the DB
insertSubmission :: UserID ->
                    ProblemID ->
                    UTCTime ->
                    Code ->
                    Qualifier -> 
                    Result ->
                    Text ->
                    AppHandler Submission
insertSubmission uid pid time code qualifier result msg = do
--  addr <- fmap (T.pack . B.toString) (getsRequest rqRemoteAddr)
  sid <- withSqlite $ \conn -> do
    S.execute conn 
      "INSERT INTO submissions \
     \ (user_id, problem_id, time, code, qualifier, result, msg) \
     \ VALUES(?, ?, ?, ?, ?, ?, ?)" (uid, pid, time, code, qualifier, result, msg)
    fmap SubmitID (S.lastInsertRowId conn)
  return (Submission sid uid pid time code qualifier result msg)
  


-- | get a single submission 
getSubmission :: UserID -> ProblemID -> SubmitID -> AppHandler Submission
getSubmission uid pid sid = do
  r <- query "SELECT * FROM submissions WHERE \
             \ id = ? AND user_id = ? AND problem_id = ?" (sid,uid,pid)
  case r of
    [s] -> return s
    _   -> notFound


-- | get all submissions for a user and problem
getSubmissions :: UserID -> ProblemID -> AppHandler [Submission]  
getSubmissions uid pid = 
  query "SELECT * FROM submissions \
       \ WHERE user_id = ? AND problem_id = ? ORDER BY id" (uid, pid)


-- | count all submissions
countSubmissions :: UserID -> ProblemID -> AppHandler Int
countSubmissions uid pid = do
  r <- listToMaybe <$>
       query "SELECT COUNT(*) \
               \ FROM submissions WHERE user_id = ? AND problem_id = ?" (uid,pid)
  return $ maybe 0 fromOnly r

-- | count submissions with a given status
countSubmissions' :: UserID -> ProblemID -> Result -> AppHandler Int
countSubmissions' uid pid result = do
  r <- listToMaybe <$>
       query "SELECT COUNT(*) \
             \ FROM submissions WHERE result = ? \
             \ AND  user_id = ? AND problem_id = ?" (result,uid,pid)
  return $ maybe 0 fromOnly r
  

-- | get the best submission for a problem for printouts:.
-- the last Accepted submission; or 
-- the last overall submission (if none was accepted)
-- Note: the query below assumes that the submissions ID key 
-- is monotonically increasing with time i.e. later submissions have higher IDs
getBestSubmission :: UserID -> ProblemID -> AppHandler (Maybe Submission)
getBestSubmission uid pid = 
  listToMaybe <$> 
  query "SELECT id,user_id,problem_id,ip_addr,time,code,status,report \
       \ FROM (SELECT *,status IN ('Accepted', 'Overdue') as accept \
             \ FROM submissions WHERE user_id = ? AND problem_id = ? \
             \ ORDER BY accept DESC, id DESC LIMIT 1)" (uid,pid) 




getWorksheetSubmissions :: UserID ->  Worksheet Problem  
                        -> AppHandler (Worksheet (Problem, [Submission]))
getWorksheetSubmissions uid (Worksheet meta items)
  = Worksheet meta <$> mapM collect items
  where collect (Left blocks) = return (Left blocks)
        collect (Right prob) = do subs <- getSubmissions uid (probID prob)
                                  return (Right (prob,subs))
        
