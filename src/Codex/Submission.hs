{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-
   Evaluating, storing and fetching submissions to/from database
-}

module Codex.Submission (
  module Codex.Submission.Types,
  Patterns,
  Ordering(..),
  insertSubmission,
  updateSubmission,
  getSubmission,
  deleteSubmission,
  getPageSubmissions,
  countPageSubmissions,
  countEarlierSubmissions,
  getPatterns,
  patternSplices,
  withSubmissions,
  withFilterSubmissions,
  filterSubmissions,
  countSubmissions,
  submitSplices,
  runSqlite
  ) where

import           Prelude hiding (Ordering)

import           Data.Map.Syntax
import           Data.Time.Clock
import           Data.Time.LocalTime

import qualified Data.Text          as T
import qualified Data.Text.Encoding as T

import           Data.List (intersperse)
import           Data.Maybe(listToMaybe, fromMaybe)

import           Snap.Core(getParam) -- hiding (path)
import           Snap.Snaplet.SqliteSimple
import           Snap.Snaplet.Router
import qualified Database.SQLite.Simple as S
import           Database.SQLite.Simple (NamedParam(..))
import           Heist.Splices     as I
import qualified Heist.Interpreted as I

import           Control.Monad (mfilter)
import           Control.Monad.Reader

import           System.FilePath (splitDirectories)

import           Codex.Application

import           Codex.Utils
import           Codex.Types
import           Codex.Tester.Result
import           Codex.Submission.Types



-- | insert a new submission into the DB
insertSubmission ::
  HasSqlite m =>
  UserLogin -> FilePath -> UTCTime -> Code -> Result -> Validity -> m Submission
insertSubmission uid path time code result val = do
  let (Code lang text) = code
  let (Result status report) = result
  withSqlite $ \conn -> do
      S.execute conn
        "INSERT INTO submissions \
         \ (user_id, path, received, language, code, status, policy, report) \
         \ VALUES(?, ?, ?, ?, ?, ?, ?, ?)"
          (uid, path, time, lang, text, status, val, report)
      sid <- fmap SubmitId (S.lastInsertRowId conn)
      return (Submission sid uid path time code result val)

-- | update submission result after evaluation
updateSubmission :: HasSqlite m => SubmitId -> Result -> Validity -> m ()
updateSubmission sid Result{..} val  =
  execute "UPDATE submissions SET status=?, policy=?, report=? \
           \ where id = ?" (resultStatus, val, resultReport, sid)


-- | get a single submission
getSubmission :: HasSqlite m => SubmitId -> m (Maybe Submission)
getSubmission sid =
  listToMaybe <$> query "SELECT * FROM submissions WHERE id=?" (Only sid)


-- | get all submissions for a user and exercise page
getPageSubmissions :: HasSqlite m => UserLogin -> FilePath -> m [Submission]
getPageSubmissions uid path =
  query "SELECT * FROM submissions \
       \ WHERE user_id = ? AND path = ? ORDER BY received" (uid, path)


-- | count user submissions to an exercise page
countPageSubmissions :: HasSqlite m => UserLogin -> FilePath -> m Int
countPageSubmissions uid path = do
  r <- query "SELECT COUNT(*) FROM submissions \
            \ WHERE user_id = ?  AND path = ?" (uid,path)
  case r of
    [Only c] -> return c
    _  -> error "countPageSubmissions: invalid result"


countEarlierSubmissions ::
  HasSqlite m => UserLogin -> FilePath -> UTCTime -> m Int
countEarlierSubmissions uid path time = do
    r <- query "SELECT COUNT(*) FROM submissions \
                \ WHERE user_id = ? AND path = ? AND received < ?"
                          (uid, path, time)
    case r of
      [Only c] -> return c
      _ -> error "countEarlierSubmissions: invalid result"


-- | delete a single submission
deleteSubmission :: HasSqlite m => SubmitId -> m ()
deleteSubmission sid =
  execute "DELETE FROM submissions where id = ?" (Only sid)

-------------------------------------------------------------------------
-- patterns for filtering submissions
type Patterns = [(Text, Maybe Text)]  -- SQL column, optional string

data Ordering
  = Ascending
  | Descending deriving (Eq, Show, Read)

-- | build a SQL query condition from patterns
sqlPatterns :: Patterns -> Text
sqlPatterns [] = ""
sqlPatterns patts
  | T.null sql = ""
  | otherwise = " WHERE " <> sql
  where sql = T.concat $
              intersperse " AND " $
              [col <> " LIKE " <> escape col | (col, Just _) <- patts]

namedParams :: Patterns -> [NamedParam]
namedParams patts = [ escape col := pat | (col, Just pat) <- patts ]

escape :: Text -> Text
escape = ("@"<>)


sqlOrdering :: Ordering -> Text
sqlOrdering Ascending  = "ASC"
sqlOrdering Descending = "DESC"

countSubmissions :: HasSqlite m => Patterns -> m Int
countSubmissions patts = withSqlite $ \conn -> do
  let sql = "SELECT COUNT(*) FROM submissions " <> sqlPatterns patts
  r <- S.queryNamed conn (S.Query sql) (namedParams patts)
  case r of
     [Only c] -> return c
     _ -> error "countSubmissions: invalid result from database"


filterSubmissions ::
  HasSqlite m => Patterns -> Ordering -> Int -> Int -> m [Submission]
filterSubmissions patts ord limit offset = 
  let
    sql = ("SELECT * FROM submissions "
            <> sqlPatterns patts
            <> " ORDER BY received " <> sqlOrdering ord
            <> " LIMIT " <> escape "limit"
            <> " OFFSET " <> escape "offset")
  in withSqlite $ \conn ->
      S.queryNamed conn (S.Query sql) ([escape "limit" := limit,
                                        escape "offset" := offset]
                                       ++ namedParams patts)
  

-- | process submissions with a filter
withFilterSubmissions :: HasSqlite m =>
  Patterns -> Ordering -> a -> (a -> Submission -> IO a) -> m a
withFilterSubmissions patts ord a f = 
  let sql = "SELECT * FROM submissions "
            <> sqlPatterns patts
            <> " ORDER BY received "
            <> sqlOrdering ord
  in withSqlite (\conn -> S.foldNamed conn (S.Query sql) (namedParams patts) a f)

-- | process all submissions
withSubmissions ::
  HasSqlite m => a -> (a -> Submission -> IO a) -> m a
withSubmissions a f = do
  let sql = "SELECT * FROM submissions ORDER BY id ASC"
  withSqlite (\conn -> S.fold_ conn sql a f)

-- | splices relating to a single submission
submitSplices :: TimeZone -> Submission -> ISplices
submitSplices tz Submission{..} = do
  "submit-id" ##  I.textSplice (T.pack $ show submitId)
  "report-url" ## urlSplice (Report submitId)
  "submission-admin-url" ## urlSplice (SubmissionAdmin submitId)
  "page-url" ## urlSplice (Page $ splitDirectories submitPath)
  "file-url" ## urlSplice (Files $ splitDirectories submitPath)
  "submit-path" ## I.textSplice (T.pack submitPath)
  "submit-user-id" ## I.textSplice (fromLogin submitUser)
  "submit-time" ## localTimeSplice tz submitTime
  "submit-lang" ## I.textSplice (fromLanguage $ codeLang submitCode)
  "submit-code" ##  I.textSplice (codeText submitCode)
  --"submit-code-url" ## I.textSplice (linkEncode (codeText submitCode))
  resultSplices submitResult
  checkSplices submitCheck

resultSplices :: Result -> ISplices
resultSplices Result{..} = do
  "result-status" ## I.textSplice (statusText resultStatus)
  "result-report" ## I.textSplice (resultReport)
  --"result-report-url" ## I.textSplice (linkEncode resultReport)
  "if-accepted" ## I.ifElseISplice (resultStatus == Accepted)
  "if-evaluating" ## I.ifElseISplice (resultStatus == Evaluating)

checkSplices :: Validity -> ISplices
checkSplices check = do
  "if-valid" ## I.ifElseISplice (check == Valid)
  "result-check" ## I.textSplice (checkText check)


statusText :: Status -> Text
statusText = T.pack . show 

checkText :: Validity -> Text
checkText Valid         = "Valid"
checkText (Invalid msg) = "Invalid: " <> msg
  


-- | Helper function to decode patterns from http request parameters
getPatterns :: Codex Patterns
getPatterns = do
  txts <- sequence [ do pat <- fmap (T.strip . T.decodeUtf8) <$> getParam field
                        return (mfilter (not . T.null) pat)
                   | field <- fields
                   ]
  return (zip (map T.decodeUtf8 fields) txts)
  where fields = ["id", "user_id", "path", "language", "status", "policy"]

patternSplices :: Patterns -> ISplices
patternSplices patts
  = sequence_ [ field ## I.textSplice (fromMaybe "" pat) | (field, pat) <- patts]

runSqlite :: Sqlite -> ReaderT Sqlite m a -> m a
runSqlite conn  = flip runReaderT conn

-- |

--linkEncode :: Text -> Text
--linkEncode t = T.decodeUtf8 $ U.urlEncode True $ T.encodeUtf8 t
