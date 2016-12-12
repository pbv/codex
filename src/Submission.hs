{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-
   Evaluating, storing and fetching and submissions in database
-}

module Submission (
  Submission(..),
  evaluate,
  getSubmission,
  getSubmissions,
  countSubmissions,
  getAll  
  ) where

-- import           System.FilePath
-- import           System.Directory
-- import           System.IO

import           Data.Time.Clock

import           Data.Maybe
import           Data.Typeable
-- import           Data.String

import qualified Data.ByteString.UTF8 as B
import           Data.Text(Text) 
import qualified Data.Text    as T
-- import qualified Data.Text.IO as T

import           Control.Monad.Trans (liftIO)

-- import           Snap.Core
import           Snap.Snaplet.SqliteSimple
import qualified Database.SQLite.Simple as S
import           Database.SQLite.Simple.FromField 
import           Database.SQLite.Simple.ToField 

import           Application
import           Utils
import           Interval
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
  result :: Result,   -- accepted/wrong answer/etc
  timing :: Timing    -- valid, early or overdue?
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




instance ToField Timing where
  toField s = toField (show s)

instance FromField Timing where
  fromField f = do s <- fromField f 
                   parse (reads s)
    where 
      parse ((s,""):_) = return s
      parse _  = returnError ConversionFailed f "invalid Timing field"
  

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
    timing <- field
    let code = Code lang text
    let result = Result classf msg
    return (Submission id uid path time code result timing)



-- | evaluate and store a new submission
-- run code tester and insert record into Db
evaluate :: UserID -> Page ->  Code -> Codex Submission
evaluate uid page@Page{..} code = do 
  now <- liftIO getCurrentTime
  env <- require getUserEvents
  let r = Interval.evalI env (Page.validInterval page) now
  case r of
    Right timing -> do
      result <- codeTester page code
      insertDb uid path now code result timing
    Left msg -> do
      insertDb uid path now code (miscError msg) Valid



-- auxliary function; insert a record into the DB
insertDb :: UserID 
  -> FilePath
  -> UTCTime
  -> Code
  -> Result
  -> Timing
  -> Codex Submission
insertDb uid path time code@(Code lang text) result@(Result classf msg) timing = do
  sid <- withSqlite $ \conn -> do
    S.execute conn 
      "INSERT INTO submissions \
       \ (user_id, path, time, language, code, class, message, timing) \
       \ VALUES(?, ?, ?, ?, ?, ?, ?, ?)" (uid, path, time, lang, text, classf, msg, timing)
    fmap SubmitID (S.lastInsertRowId conn)
  return (Submission sid uid path time code result timing)

 

-- | get a single submission 
getSubmission :: SubmitID -> Codex Submission
getSubmission sid = do
  r <- query "SELECT * FROM submissions WHERE id = ?" (Only sid)
  case r of
    [s] -> return s
    _   -> notFound


-- | get all user submissions for a user and path
getAll :: UserID -> FilePath -> Codex [Submission]  
getAll uid path = 
  query "SELECT * FROM submissions \
       \ WHERE user_id = ? AND path = ? ORDER BY id" (uid, path)


-------------------------------------------------------------------------
type Pattern = Text

countSubmissions :: [Pattern] -> Codex Int
countSubmissions patts = do
  let [id,uid,path,time,lang,classf,timing] = map checkEmpty patts
  r <- query "SELECT COUNT(*) FROM submissions WHERE \
        \ id LIKE ? AND user_id LIKE ? AND path LIKE ? AND time LIKE ? \
        \ AND language LIKE ? AND class LIKE ? AND timing LIKE ?" 
        (id,uid,path,time,lang,classf,timing)
  case r of 
     [Only c] -> return c
     _ -> error "countSubmissions failed; this should NOT have happened!"


getSubmissions :: [Pattern] -> Int -> Int -> Codex [Submission]
getSubmissions patts limit offset = do
  let [id,uid,path,time,lang,classf,timing] = map checkEmpty patts
  query "SELECT * FROM submissions WHERE \
        \ id LIKE ? AND user_id LIKE ? AND path LIKE ? AND time LIKE ? \
        \ AND language LIKE ? AND class LIKE ? AND timing LIKE ? \
        \ ORDER BY id ASC LIMIT ? OFFSET ?" 
        (id,uid,path,time,lang,classf,timing,limit,offset)
  

checkEmpty :: Pattern -> Pattern
checkEmpty p = let q = T.strip p
               in if T.null q then "%" else q
  


                                      
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


