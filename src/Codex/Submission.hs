{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-
   Evaluating, storing and fetching submissions to/from database
-}

module Codex.Submission (
  Submission(..),
  Patterns,
  Ordering(..),
  insertSubmission,
  updateSubmission,
  markEvaluating,
  getSubmission,
  deleteSubmission,
  getPageSubmissions,
  countPageSubmissions,
  getPatterns,
  patternSplices,
  withSubmissions,
  withFilterSubmissions,
  filterSubmissions,
  countSubmissions,
  submitSplices,
  submitLang
  ) where

import           Prelude hiding (Ordering)

import           Data.Map.Syntax
import           Data.Time.Clock
import           Data.Time.LocalTime

import qualified Data.Text          as T
import qualified Data.Text.Encoding as T

import           Data.Monoid
import           Data.List (intersperse)
import           Data.Maybe(listToMaybe, fromMaybe)

import           Snap.Core hiding (path)
import           Snap.Snaplet.SqliteSimple
import           Snap.Snaplet.Router
import qualified Database.SQLite.Simple as S
import           Database.SQLite.Simple (NamedParam(..))
import           Database.SQLite.Simple.FromField
import           Database.SQLite.Simple.ToField
import           Heist.Splices     as I
import qualified Heist.Interpreted as I

import           Control.Concurrent.MVar
import           Control.Monad (mfilter)

import           System.FilePath (splitDirectories)

import           Codex.Application

import           Codex.Utils
import           Codex.Interval
import           Codex.Types
import           Codex.Tester.Result



-- | a row in the submssion table
data Submission = Submission {
  submitId     :: SubmitId,     -- submission DB id
  submitUser   :: UserLogin,    -- user login
  submitPath   :: FilePath,     -- exercise request path 
  submitTime   :: UTCTime,      -- submission time
  submitCode   :: Code,         -- submitted code
  submitResult :: Result,       -- accepted/wrong answer/etc
  submitTiming :: Timing        -- timing (early, valid, overdue)
  } 


submitLang :: Submission -> Language
submitLang = codeLang . submitCode


instance ToField Timing where
  toField s = toField (show s)

instance FromField Timing where
  fromField f = do s <- fromField f
                   parse (reads s)
    where
      parse ((s,""):_) = return s
      parse _  = returnError ConversionFailed f "invalid Timing field"

instance FromRow Submission where
  fromRow = do
    sid <- field
    uid <- field
    path <- field
    time <- field
    lang <- field
    text <- field 
    classf <- field
    msg <- field
    tv <- field
    let code = Code lang text
    let result = Result classf msg
    return (Submission sid uid path time code result tv)



-- | insert a new submission into the DB
insertSubmission ::
  UserLogin -> FilePath -> UTCTime -> Code -> Result -> Timing ->
  Codex Submission
insertSubmission uid path time code result timing = do
  let (Code lang text) = code
  let (Result classf msg) = result
  withSqlite $ \conn -> do
    S.execute conn
      "INSERT INTO submissions \
       \ (user_id, path, received, language, code, class, message, timing) \
       \ VALUES(?, ?, ?, ?, ?, ?, ?, ?)"
       (uid, path, time, lang, text, classf, msg, timing)
    sid <- fmap SubmitId (S.lastInsertRowId conn)
    return (Submission sid uid path time code result timing)

-- | update submission result after evaluation
updateSubmission :: Sqlite -> SubmitId -> Result -> Timing -> IO ()
updateSubmission sqlite sid result timing =
  withMVar (sqliteConn sqlite) $ \conn ->
     S.execute conn
       "UPDATE submissions SET class=?, message=?, timing=? \
       \ where id = ?" (resultClassify result, resultMessage result, timing, sid)


-- | mark submissions to the "evaluating" state
markEvaluating :: Sqlite -> [SubmitId] -> IO ()
markEvaluating sqlite sids = do
  withMVar (sqliteConn sqlite) $ \conn ->
    S.withTransaction conn $
    S.executeMany conn
     "UPDATE submissions SET class='Evaluating' where id=?" (map Only sids)



-- | get a single submission
getSubmission :: SubmitId -> Codex (Maybe Submission)
getSubmission sid =
  listToMaybe <$> query "SELECT * FROM submissions WHERE id=?" (Only sid)


-- | get all submissions for a user and exercise page
getPageSubmissions :: UserLogin -> FilePath -> Codex [Submission]
getPageSubmissions uid path =
  query "SELECT * FROM submissions \
       \ WHERE user_id = ? AND path = ? ORDER BY id" (uid, path)


-- | count user submissions to an exercise page
countPageSubmissions :: UserLogin -> FilePath -> Codex Int
countPageSubmissions uid path = do
  r <- query "SELECT COUNT(*) FROM submissions \
            \ WHERE user_id = ?  AND path = ?" (uid,path)
  case r of
    [Only c] -> return c
    _  -> error "countPageSubmissions: invalid result from database"


-- | delete a single submission
deleteSubmission :: SubmitId -> Codex ()
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

countSubmissions :: Patterns -> Codex Int
countSubmissions patts = withSqlite $ \conn -> do
  let sql = "SELECT COUNT(*) FROM submissions " <> sqlPatterns patts
  r <- S.queryNamed conn (S.Query sql) (namedParams patts)
  case r of
     [Only c] -> return c
     _ -> error "countSubmissions: invalid result from database"


filterSubmissions :: Patterns -> Ordering -> Int -> Int -> Codex [Submission]
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
withFilterSubmissions ::
  Patterns -> Ordering -> a -> (a -> Submission -> IO a) -> Codex a
withFilterSubmissions patts ord a f = 
  let sql = "SELECT * FROM submissions "
            <> sqlPatterns patts
            <> " ORDER BY received "
            <> sqlOrdering ord
  in withSqlite (\conn -> S.foldNamed conn (S.Query sql) (namedParams patts) a f)

-- | process all submissions
withSubmissions :: a -> (a -> Submission -> IO a) -> Codex a
withSubmissions a f = do
  let sql = "SELECT * FROM submissions ORDER BY id ASC"
  withSqlite (\conn -> S.fold_ conn sql a f)


-- | Helper function to decode patterns from http request parameters
getPatterns :: Codex Patterns
getPatterns = do
  txts <- sequence [ do pat <- fmap (T.strip . T.decodeUtf8) <$> getParam field
                        return (mfilter (not . T.null) pat)
                   | field <- fields
                   ]
  return (zip (map T.decodeUtf8 fields) txts)
  where fields = ["id", "user_id", "path", "language", "class", "timing"]

patternSplices :: Patterns -> ISplices
patternSplices patts
  = sequence_ [ field ## I.textSplice (fromMaybe "" pat) | (field, pat) <- patts]



-- | splices relating to a single submission
submitSplices :: TimeZone -> Submission -> ISplices
submitSplices tz Submission{..} = do
  "submit-id" ##  I.textSplice (toText submitId)
  "report-url" ## urlSplice (Report submitId)
  "submission-admin-url" ## urlSplice (SubmissionAdmin submitId)
  "page-url" ## urlSplice (Page $ splitDirectories submitPath)
  "file-url" ## urlSplice (Files $ splitDirectories submitPath)
  "submit-path" ## I.textSplice (T.pack submitPath)
  "submit-user-id" ## I.textSplice (toText submitUser)
  "submit-time" ## utcTimeSplice tz submitTime
  "submit-lang" ## I.textSplice (toText $ codeLang submitCode)
  "submit-text" ##  I.textSplice (codeText submitCode)
  let classify = T.pack $ show $ resultClassify submitResult
  "submit-classify" ##  I.textSplice classify
  "submit-message" ## I.textSplice (resultMessage submitResult)
  "submit-timing" ## I.textSplice (T.pack $ show submitTiming)
  "valid" ## I.ifElseISplice (submitTiming == Valid)
  "early" ## I.ifElseISplice (submitTiming == Early)
  "overdue" ## I.ifElseISplice (submitTiming == Overdue)
  "accepted" ## I.ifElseISplice (resultClassify submitResult == Accepted)
  "evaluating" ## I.ifElseISplice (resultClassify submitResult == Evaluating)
                                      


