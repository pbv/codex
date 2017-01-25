{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-
   Evaluating, storing and fetching submissions to/from database
-}

module Codex.Submission (
  Submission(..),
  Patterns(..),
  Sorting(..),
  insertSubmission,
  updateSubmission,
  markEvaluating,
  getSubmission,
  deleteSubmission,
  getPatterns,
  normalizePatterns,
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


import           Data.Map.Syntax
import           Data.Time.Clock
import           Data.Time.LocalTime

import qualified Data.Text          as T
import qualified Data.Text.Encoding as T

import           Data.Maybe(listToMaybe)

import           Snap.Snaplet.SqliteSimple
import qualified Database.SQLite.Simple as S
import           Database.SQLite.Simple.FromField
import           Database.SQLite.Simple.ToField
import           Heist.Splices     as I
import qualified Heist.Interpreted as I

import           Control.Concurrent.MVar

import           Codex.Application

import           Codex.Utils
import           Codex.Interval
import           Codex.Types
import           Codex.Tester.Result



-- | a row in the submssion table
data Submission = Submission {
  submitId     :: SubmitId,    -- submission DB id
  submitUser   :: UserLogin,    -- user login
  submitPath   :: FilePath,  -- exercise path
  submitTime   :: UTCTime,      -- submition time
  submitCode   :: Code,       -- program code
  submitResult :: Result,   -- accepted/wrong answer/etc
  submitTiming :: Timing    -- valid, early or overdue?
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
  UserLogin -> FilePath ->  UTCTime -> Code -> Result -> Timing
  -> Codex Submission
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
type Pattern = Text

data Patterns = Patterns {
  idPat :: Pattern,
  userPat :: Pattern,
  pathPat :: Pattern,
  langPat :: Pattern,
  classPat :: Pattern,
  timingPat :: Pattern
  } deriving Show


data Sorting = Asc | Desc deriving (Eq, Show, Read)


countSubmissions :: Patterns -> Codex Int
countSubmissions Patterns{..} = do
  r <- query "SELECT COUNT(*) FROM submissions WHERE \
        \ id LIKE ? AND user_id LIKE ? AND path LIKE ? \
        \ AND language LIKE ? AND class LIKE ? AND timing LIKE ?"
        (idPat, userPat, pathPat, langPat, classPat, timingPat)
  case r of
     [Only c] -> return c
     _ -> error "countSubmissions failed"


filterSubmissions :: Patterns -> Sorting -> Int -> Int -> Codex [Submission]
filterSubmissions Patterns{..} sort limit offset = 
  let
    patts = (idPat, userPat, pathPat, langPat, classPat, timingPat, limit, offset)
    sql = case sort of
        Asc -> "SELECT * FROM submissions WHERE \
        \ id LIKE ? AND user_id LIKE ? AND path LIKE ? \
        \ AND language LIKE ? AND class LIKE ? AND timing LIKE ? \
        \ ORDER BY received ASC LIMIT ? OFFSET ?"
        Desc -> "SELECT * FROM submissions WHERE \
        \ id LIKE ? AND user_id LIKE ? AND path LIKE ? \
        \ AND language LIKE ? AND class LIKE ? AND timing LIKE ? \
        \ ORDER BY received DESC LIMIT ? OFFSET ?"
    in query sql patts



-- | process submissions with a filter
withFilterSubmissions :: Patterns -> a -> (a -> Submission -> IO a) -> Codex a
withFilterSubmissions Patterns{..} a f = 
  let patts = (idPat, userPat, pathPat, langPat, classPat, timingPat)
      q = "SELECT * FROM submissions WHERE \
        \ id LIKE ? AND user_id LIKE ? AND path LIKE ? \
        \ AND language LIKE ? AND class LIKE ? AND timing LIKE ? \
        \ ORDER BY received ASC"
  in withSqlite (\conn -> S.fold conn q patts a f)

-- | process all submissions
withSubmissions :: a -> (a -> Submission -> IO a) -> Codex a
withSubmissions a f = do
  let q = "SELECT * FROM submissions ORDER BY id ASC"
  withSqlite (\conn -> S.fold_ conn q a f)


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


-- | normalize patterns
-- remove whitespace and replace empty strings with SQL wilcards `%'
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





-- | splices relating to a single submission
submitSplices :: TimeZone -> Submission -> ISplices
submitSplices tz Submission{..} = do
  "submit-id" ##  I.textSplice (toText submitId)
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
