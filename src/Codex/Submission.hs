{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-
   Evaluating, storing and fetching submissions to/from database
-}

module Codex.Submission (
  Submission(..),
  Patterns(..),
  Ordering(..),
  insertSubmission,
  updateSubmission,
  markEvaluating,
  getSubmission,
  deleteSubmission,
  getPatterns,
  patternSplices,
  withSubmissions,
  withFilterSubmissions,
  filterSubmissions,
  countSubmissions,
  getPageSubmissions,
  submitSplices,
  getLastAccepted,
  getLastSubmitted,
  querySubmissionPaths,
  querySubmissionUsers
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

import           Snap.Core
import           Snap.Snaplet.SqliteSimple
import           Snap.Snaplet.Router
import qualified Database.SQLite.Simple as S
import           Database.SQLite.Simple (NamedParam(..))
import           Database.SQLite.Simple.FromField
import           Database.SQLite.Simple.ToField
import           Heist.Splices     as I
import qualified Heist.Interpreted as I

import           Control.Concurrent.MVar
import           Control.Monad.State (liftIO)

import           Codex.Application

import           Codex.Utils
import           Codex.Interval
import           Codex.Types
import           Codex.Tester.Result



-- | a row in the submssion table
data Submission = Submission {
  submitId     :: SubmitId,     -- submission DB id
  submitUser   :: UserLogin,    -- user login
  submitPath   :: FilePath,     -- exercise path
  submitTime   :: UTCTime,      -- submition time
  submitCode   :: Code,         -- program code
  submitResult :: Result,       -- accepted/wrong answer/etc
  submitTiming :: Timing        -- timing (early, valid, overdue)
  } 


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
  UserLogin -> FilePath -> UTCTime -> Code ->  Result -> Timing ->
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

-- | delete a single submission
deleteSubmission :: SubmitId -> Codex ()
deleteSubmission sid =
  execute "DELETE FROM submissions where id = ?" (Only sid)

-------------------------------------------------------------------------
-- patterns for filtering submissions

{-
data Patterns = Patterns {
  idPat :: Pattern,
  userPat :: Pattern,
  pathPat :: Pattern,
  langPat :: Pattern,
  classPat :: Pattern,
  timingPat :: Pattern
  } deriving Show
-}


type Patterns = [(Text, Maybe Text)]  -- SQL column, optional string

data Ordering
  = Ascending
  | Descending deriving (Eq, Show, Read)

-- | build a SQL query condition from patterns
queryPatterns :: Patterns -> Text
queryPatterns [] = ""
queryPatterns patts
  | T.null cond = ""
  | otherwise = " WHERE " <> cond
  where cond = T.concat $
               intersperse " AND " $
               [col <> " LIKE " <> escape col | (col, Just _) <- patts]

namedParams :: Patterns -> [NamedParam]
namedParams patts = [ escape col := pat | (col, Just pat) <- patts ]

escape :: Text -> Text
escape = ("@"<>)


countSubmissions :: Patterns -> Codex Int
countSubmissions patts = withSqlite $ \conn -> do
  let sql = "SELECT COUNT(*) FROM submissions " <> queryPatterns patts
  r <- S.queryNamed conn (S.Query sql) (namedParams patts)
  case r of
     [Only c] -> return c
     _ -> return 0 -- error!!!


filterSubmissions :: Patterns -> Ordering -> Int -> Int -> Codex [Submission]
filterSubmissions patts sort limit offset = 
  let
    ord = case sort of
      Ascending -> "ASC"
      Descending -> "DESC"
    sql = ("SELECT * FROM submissions "
            <> queryPatterns patts
            <> " ORDER BY received " <> ord
            <> " LIMIT " <> escape "limit"
            <> " OFFSET " <> escape "offset")
  in do
    liftIO $ print sql
    liftIO $ print patts
    withSqlite $ \conn ->
      S.queryNamed conn (S.Query sql) ([escape "limit" := limit,
                                        escape "offset" := offset]
                                       ++ namedParams patts)
  


-- | process submissions with a filter
withFilterSubmissions :: Patterns -> a -> (a -> Submission -> IO a) -> Codex a
withFilterSubmissions patts a f = 
  let sql = "SELECT * FROM submissions "
            <> queryPatterns patts
            <> " ORDER  BY received ASC"
  in withSqlite (\conn -> S.foldNamed conn (S.Query sql) (namedParams patts) a f)

-- | process all submissions
withSubmissions :: a -> (a -> Submission -> IO a) -> Codex a
withSubmissions a f = do
  let sql = "SELECT * FROM submissions ORDER BY id ASC"
  withSqlite (\conn -> S.fold_ conn sql a f)

{-
-- | Helper function to decode patterns from an http request parameters
getPatterns :: Codex Patterns
getPatterns = do
  id_pat <-   T.decodeUtf8 <$> getParamDef "id_pat" ""
  uid_pat <-  T.decodeUtf8 <$> getParamDef "uid_pat" ""
  path_pat<-  T.decodeUtf8 <$> getParamDef "path_pat" ""
  lang_pat<-  T.decodeUtf8 <$> getParamDef "lang_pat" ""
  class_pat<- T.decodeUtf8 <$> getParamDef "class_pat" ""
  timing_pat<- T.decodeUtf8 <$> getParamDef "timing_pat" ""
  return Patterns { idPat = id_pat, userPat = uid_pat,
                    pathPat = path_pat, langPat = lang_pat,
                    classPat = class_pat, timingPat = timing_pat }
-}

-- | Helper function to decode patterns from an http request parameters
getPatterns :: Codex Patterns
getPatterns = do
  sequence [ do pat <- getParam field 
                return (T.decodeUtf8 field, fmap T.decodeUtf8 pat)
           | field <- ["id", "user_id", "path", "language", "class", "timing"]
           ]

patternSplices :: Patterns -> ISplices
patternSplices patts
  = sequence_ [ field ## I.textSplice (fromMaybe "" pat)
              | (field, pat) <- patts]

{-
patternSplices :: Patterns -> ISplices
patternSplices patts = do
  let id = fromMaybe "" (lookup "id" patts)
  let user = fromMaybe "" (lookup "user_id" patts)
  let lang = fromMaybe "" (lookup "language" patts)
  let path = fromMaybe "" (lookup "path" patts)
  let clss = fromMaybe "" (lookup "class" patts)
  let timing = fromMaybe "" (lookup "timing" patts)
  "id_pat" ## I.textSplice id
  "uid_pat" ## I.textSplice user
  "path_pat" ## I.textSplice path
  "lang_pat" ## I.textSplice lang
  "class_pat" ## I.textSplice clss
  "timing_pat" ## I.textSplice timing
-}



{-
normalizePatterns :: Patterns -> Patterns
normalizePatterns Patterns{..}
  = Patterns { idPat = normal idPat,
               userPat = normal userPat,
               pathPat =normal pathPat,
               langPat = normal langPat,
               classPat = normal classPat,
               timingPat = normal timingPat
             }
  where normal :: Pattern -> Pattern
        normal pat = let pat'=T.strip pat in if T.null pat' then "%" else pat'
-}




-- | splices relating to a single submission
submitSplices :: TimeZone -> Submission -> ISplices
submitSplices tz Submission{..} = do
  "submit-id" ##  I.textSplice (toText submitId)
  "submit-url" ## urlSplice (Report submitId)
  "submit-user-id" ## I.textSplice (toText submitUser)
  "submit-path" ## I.textSplice (T.decodeUtf8 $ encodePath submitPath)
  "submit-time" ## utcTimeSplice tz submitTime
  "code-lang" ## I.textSplice (toText $ codeLang submitCode)
  "code-text" ##  I.textSplice (codeText submitCode)
  let classify = T.pack $ show $ resultClassify submitResult
  "submit-classify" ##  I.textSplice classify
  "submit-message" ## I.textSplice (resultMessage submitResult)
  "submit-timing" ## I.textSplice (T.pack $ show submitTiming)
  "valid" ## I.ifElseISplice (submitTiming == Valid)
  "early" ## I.ifElseISplice (submitTiming == Early)
  "overdue" ## I.ifElseISplice (submitTiming == Overdue)
  "accepted" ## I.ifElseISplice (resultClassify submitResult == Accepted)
  "evaluating" ## I.ifElseISplice (resultClassify submitResult == Evaluating)
                                      



getLastAccepted :: UserLogin -> FilePath -> Codex (Maybe Submission)
getLastAccepted uid path 
  = listToMaybe <$>
    query "SELECT * FROM submissions WHERE \
          \ user_id = ? AND path = ? AND class='Accepted' \
          \ ORDER BY id DESC LIMIT 1" (uid, path)

getLastSubmitted :: UserLogin -> FilePath -> Codex (Maybe Submission)
getLastSubmitted uid path 
  = listToMaybe <$>
    query "SELECT * FROM submissions WHERE \
          \ user_id = ? AND path = ? \
          \ ORDER BY id DESC LIMIT 1" (uid, path)
  

querySubmissionPaths :: UserLogin -> Codex [FilePath]
querySubmissionPaths uid
  = map T.unpack <$>
    query "SELECT DISTINCT path FROM submissions \
          \ WHERE user_id = ? ORDER BY path" (Only uid)

querySubmissionUsers :: Codex [UserLogin]
querySubmissionUsers
  = query_ "SELECT DISTINCT user_id FROM submissions ORDER BY user_id"
