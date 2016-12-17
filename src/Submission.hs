{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-
   Evaluating, storing and fetching submissions to/from database
-}

module Submission (
  Submission(..),
  Patterns(..),
  Sorting(..),
  insertSubmission,
  getSubmission,
  deleteSubmission,
  emptyPatterns,
  withSubmissions,
  withFilterSubmissions,
  filterSubmissions,
  countSubmissions,
  getPageSubmissions,
  submitSplices   
  ) where


import           Data.Map.Syntax
import           Data.Time.Clock
import           Data.Time.LocalTime

import           Data.Text(Text) 
import qualified Data.Text          as T
import qualified Data.Text.Encoding as T

import           Data.Maybe(listToMaybe)

import           Snap.Snaplet.SqliteSimple
import qualified Database.SQLite.Simple as S
import           Database.SQLite.Simple.FromField 
import           Database.SQLite.Simple.ToField 
import           Heist.Splices     as I
import qualified Heist.Interpreted as I

 

import           Application
import           Utils
import           Interval
import           Types
import           Language
import           Tester


-- | a row in the submssion table
data Submission = Submission {
  submitID :: SubmitID,     -- submission id
  submitUser :: UserID,    -- user id
  submitPath  :: FilePath,  -- exercise path 
  submitTime :: UTCTime,      -- submition time  
  submitCode :: Code,       -- program code
  submitResult :: Result,   -- accepted/wrong answer/etc
  submitTiming :: Timing    -- valid, early or overdue?
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
    received <- field
    lang <- field
    text <- field
    classf <- field
    msg <- field
    timing <- field
    let code = Code lang text
    let result = Result classf msg
    return (Submission id uid path received code result timing)







-- | insert a new submission into the DB
insertSubmission ::
  UserID -> FilePath -> UTCTime -> Code -> Result -> Timing -> Codex Submission
insertSubmission uid path received code result timing = do
  let (Code lang text) = code
  let (Result classf msg) = result
  sid <- withSqlite $ \conn -> do
    S.execute conn 
      "INSERT INTO submissions \
       \ (user_id, path, received, language, code, class, message, timing) \
       \ VALUES(?, ?, ?, ?, ?, ?, ?, ?)" (uid, path, received, lang, text, classf, msg, timing)
    fmap SubmitID (S.lastInsertRowId conn)
  return (Submission sid uid path received code result timing)


-- | get a single submission 
getSubmission :: SubmitID -> Codex (Maybe Submission)
getSubmission sid = 
  listToMaybe <$> query "SELECT * FROM submissions WHERE id = ?" (Only sid)


-- | get all submissions for a user and exercise page
getPageSubmissions :: UserID -> FilePath -> Codex [Submission]  
getPageSubmissions uid path = 
  query "SELECT * FROM submissions \
       \ WHERE user_id = ? AND path = ? ORDER BY id" (uid, path)

-- | delete a single submission
deleteSubmission :: SubmitID -> Codex ()
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
  } 

emptyPatterns :: Patterns
emptyPatterns = Patterns {idPat = "", userPat = "", pathPat = "",
                          langPat = "", classPat = "", timingPat = ""}



checkEmpty :: Pattern -> Pattern
checkEmpty p = let q=T.strip p in if T.null q then "%" else q

checkEmptyPat :: Patterns -> Patterns
checkEmptyPat Patterns{..}
  = Patterns { idPat = checkEmpty idPat,
               userPat = checkEmpty userPat,
               pathPat =checkEmpty pathPat,
               langPat = checkEmpty langPat,
               classPat = checkEmpty classPat,
               timingPat = checkEmpty timingPat
             }
    

data Sorting = Asc | Desc deriving (Eq, Show, Read)


countSubmissions :: Patterns -> Codex Int
countSubmissions patts = do
  let Patterns{..} = checkEmptyPat patts
  r <- query "SELECT COUNT(*) FROM submissions WHERE \
        \ id LIKE ? AND user_id LIKE ? AND path LIKE ? \
        \ AND language LIKE ? AND class LIKE ? AND timing LIKE ?" 
        (idPat, userPat, pathPat, langPat, classPat, timingPat)
  case r of 
     [Only c] -> return c
     _ -> error "countSubmissions failed"


filterSubmissions :: Patterns -> Sorting -> Int -> Int -> Codex [Submission]
filterSubmissions patts sort limit offset = do
  let Patterns{..} = checkEmptyPat patts
  let sql = case sort of
        Asc -> "SELECT * FROM submissions WHERE \
        \ id LIKE ? AND user_id LIKE ? AND path LIKE ? \
        \ AND language LIKE ? AND class LIKE ? AND timing LIKE ? \
        \ ORDER BY received ASC LIMIT ? OFFSET ?" 
        Desc -> "SELECT * FROM submissions WHERE \
        \ id LIKE ? AND user_id LIKE ? AND path LIKE ? \
        \ AND language LIKE ? AND class LIKE ? AND timing LIKE ? \
        \ ORDER BY received DESC LIMIT ? OFFSET ?" 
  query sql (idPat, userPat, pathPat, langPat, classPat, timingPat, limit, offset)


  
-- | process submissions with a filter
withFilterSubmissions :: Patterns -> a -> (a -> Submission -> IO a) -> Codex a
withFilterSubmissions patts a f = do
  let Patterns{..} = checkEmptyPat patts
  let patts' = (idPat, userPat, pathPat, langPat, classPat, timingPat)
  let q = "SELECT * FROM submissions WHERE \
        \ id LIKE ? AND user_id LIKE ? AND path LIKE ? \
        \ AND language LIKE ? AND class LIKE ? AND timing LIKE ? \
        \ ORDER BY received ASC"
  withSqlite (\conn -> S.fold conn q patts' a f)

-- | process all submissions
withSubmissions :: a -> (a -> Submission -> IO a) -> Codex a
withSubmissions a f = do
  let q = "SELECT * FROM submissions ORDER BY id ASC"
  withSqlite (\conn -> S.fold_ conn q a f)


                                      
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



-- | splices relating to a single submission
submitSplices :: TimeZone -> Submission -> ISplices
submitSplices tz Submission{..} = do
  "submit-id" ##  I.textSplice (toText submitID)
  "submit-user-id" ## I.textSplice (toText submitUser)
  "submit-path" ## I.textSplice (T.decodeUtf8 $ encodePath submitPath)
  "received" ## utcTimeSplice tz submitTime
  "code-lang" ## I.textSplice (fromLanguage $ codeLang submitCode)
  "code-text" ##  I.textSplice (codeText submitCode)
  "classify" ##  I.textSplice (T.pack $ show $ resultClassify submitResult)
  "message" ## I.textSplice (resultMessage submitResult)
  "case-timing" ## caseSplice submitTiming
  "timing" ## I.textSplice (T.pack $ show submitTiming)
  "valid" ## I.ifElseISplice (submitTiming == Valid)
  "early" ## I.ifElseISplice (submitTiming == Early)
  "overdue" ## I.ifElseISplice (submitTiming == Overdue)
  "accepted" ## I.ifElseISplice (resultClassify submitResult == Accepted)
