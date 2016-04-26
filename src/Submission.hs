{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-
   Evaluating, storing and fetching and submissions in database
-}

module Submission (
  Submission(..),
  evaluate,
  getSingle,
  getAll  
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
import           Page
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
  toField (Language l) = toField l

instance FromField Language where
  fromField f = Language <$> fromField f


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



-- | evaluate and store a new submission
evaluate :: UserID -> Page ->  Code -> AppHandler Submission
evaluate uid page@Page{..} code = do 
    result <- codeTester page code
    -- now <- liftIO getCurrentTime
    -- let qualifier = if now `before` probLimit then OK else Overdue
    insertDb uid path code result 

-- insert into the DB
insertDb :: UserID -> FilePath  -> Code -> Result -> AppHandler Submission
insertDb uid path code@(Code lang text) result@(Result classf msg) = do
  time <- liftIO getCurrentTime
  sid <- withSqlite $ \conn -> do
    S.execute conn 
      "INSERT INTO submissions \
     \ (user_id, path, time, language, code, class, message) \
     \ VALUES(?, ?, ?, ?, ?, ?, ?)" (uid, path, time, lang, text, classf, msg)
    fmap SubmitID (S.lastInsertRowId conn)
  return (Submission sid uid path time code result)
  


-- | get a single submission 
getSingle :: UserID -> SubmitID -> AppHandler Submission
getSingle uid sid = do
  r <- query "SELECT * FROM submissions WHERE \
             \ id = ? AND user_id = ?" (sid,uid)
  case r of
    [s] -> return s
    _   -> notFound


-- | get all user submissions for a  path
getAll :: UserID -> FilePath -> AppHandler [Submission]  
getAll uid path = 
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


