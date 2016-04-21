{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-
   Evaluating, storing and fetching and submissions in database
-}

module Submission where

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
  id :: SubmitID,     -- submission id
  userID  :: UserID,    -- user id
  path  :: FilePath,  -- problem path
  time :: UTCTime,    -- submit time  
  code :: Code,       -- program code
  result :: Result    -- accepted/wrong answer/etc
  }


-- | convertions to/from SQL 
instance ToField Classify where
  toField s = toField (show s)

instance FromField Classify where
  fromField f = do s <- fromField f 
                   parse (reads s)
    where 
      parse ((s,""):_) = return s
      parse _  = returnError ConversionFailed f "invalid Classify field"


instance ToField Language where
  toField s = toField (show s)

instance FromField Language where
  fromField f = do s <- fromField f 
                   parse (reads s)
    where 
      parse ((s,""):_) = return s
      parse _  = returnError ConversionFailed f "invalid Language field"
  

instance FromRow Submission where
  fromRow = do
    id <- field
    uid <- field
    path <- field
    time <- field
    lang <- field
    text <- field
    classf <- field
    msg <- field
    let code = Code lang text
    let result = Result classf msg
    return (Submission id uid path time code result)



-- | evaluate a new submission
newSubmission :: Page -> UserID -> Code -> AppHandler Submission
newSubmission page@Page{..} uid code = do 
    result <- codeTester page code
    -- now <- liftIO getCurrentTime
    -- let qualifier = if now `before` probLimit then OK else Overdue
    insertSubmission uid path code result 

-- before :: Ord t => t -> Maybe t -> Bool
-- before now limit = maybe True (now<=) limit
          


-- | insert a new submission into the DB
insertSubmission ::
  UserID -> FilePath  -> Code -> Result -> AppHandler Submission
insertSubmission uid path code@(Code lang text) result@(Result classf msg) = do
  time <- liftIO getCurrentTime
  sid <- withSqlite $ \conn -> do
    S.execute conn 
      "INSERT INTO submissions \
     \ (user_id, path, time, language, code, class, message) \
     \ VALUES(?, ?, ?, ?, ?, ?, ?)" (uid, path, time, lang, text, classf, msg)
    fmap SubmitID (S.lastInsertRowId conn)
  return (Submission sid uid path time code result)
  


-- | get a single submission 
getSubmission :: UserID -> SubmitID -> AppHandler Submission
getSubmission uid sid = do
  r <- query "SELECT * FROM submissions WHERE \
             \ id = ? AND user_id = ?" (sid,uid)
  case r of
    [s] -> return s
    _   -> notFound


-- | get all user submissions for a  path
getSubmissions :: UserID -> FilePath -> AppHandler [Submission]  
getSubmissions uid path = 
  query "SELECT * FROM submissions \
       \ WHERE user_id = ? AND path = ? ORDER BY id" (uid, path)

{-
-- | count all submissions
countSubmissions :: UserID -> FilePath -> AppHandler Int
countSubmissions uid path = do
  r <- listToMaybe <$>
       query "SELECT COUNT(*) \
               \ FROM submissions WHERE user_id = ? AND path = ?" (uid,path)
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
-}


