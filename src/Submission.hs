{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-
   Evaluating, storing and fetching submissions to/from database
-}

module Submission (
  Submission(..),
  Patterns(..),
  Sorting(..),
  -- evaluate,
  insert,
  get,
  delete,
  emptyPat,
  withSubmissions,
  withFilterSubmissions,
  filterSubmissions,
  countSubmissions,
  getReport,
  exportCSV
  ) where


import           System.Directory
import           System.IO

import           Data.Time.Clock
import           Data.Time.LocalTime

import           Data.List(intersperse)

import           Data.Text(Text) 
import qualified Data.Text    as T

import           Control.Monad.Trans (liftIO)

import           Snap.Snaplet.SqliteSimple
import qualified Database.SQLite.Simple as S
import           Database.SQLite.Simple.FromField 
import           Database.SQLite.Simple.ToField 

import           Application
import           Utils
import           Interval
import           Types
import           Language
import           Tester


-- | a row in the submssion table
data Submission = Submission {
  id :: SubmitID,     -- submission id
  userID  :: UserID,    -- user id
  path  :: FilePath,  -- exercise path
  received :: UTCTime,    -- submit time  
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
    received <- field
    lang <- field
    text <- field
    classf <- field
    msg <- field
    timing <- field
    let code = Code lang text
    let result = Result classf msg
    return (Submission id uid path received code result timing)




-- worker function to insert a new submission into the DB
insert :: UserID -> FilePath -> UTCTime -> Code -> Result -> Timing 
            -> Codex Submission
insert uid path received code result timing = do
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
get :: SubmitID -> Codex Submission
get sid = do
  r <- query "SELECT * FROM submissions WHERE id = ?" (Only sid)
  case r of
    [s] -> return s
    _   -> notFound


-- | get all user submissions for a user and path
getReport :: UserID -> FilePath -> Codex [Submission]  
getReport uid path = 
  query "SELECT * FROM submissions \
       \ WHERE user_id = ? AND path = ? ORDER BY id" (uid, path)

-- | delete a single submission
delete :: SubmitID -> Codex ()
delete sid =
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
  } deriving (Eq, Show)

emptyPat :: Patterns
emptyPat = Patterns {idPat = "", userPat = "", pathPat = "",
                     langPat = "", classPat = "", timingPat = ""}

data Sorting = Asc | Desc deriving (Eq, Show, Read)


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
    

countSubmissions :: Patterns -> Codex Int
countSubmissions patts = do
  let Patterns{..} = checkEmptyPat patts
  r <- query "SELECT COUNT(*) FROM submissions WHERE \
        \ id LIKE ? AND user_id LIKE ? AND path LIKE ? \
        \ AND language LIKE ? AND class LIKE ? AND timing LIKE ?" 
        (idPat, userPat, pathPat, langPat, classPat, timingPat)
  case r of 
     [Only c] -> return c
     _ -> error "countSubmissions failed; this should NOT have happened!"


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


-- | export all submissions to a CSV text file
-- NB: the caller should remove the temporary file 
exportCSV :: FilePath -> String -> Codex FilePath
exportCSV filetpl sep  = do
  tmpDir <- liftIO getTemporaryDirectory
  (path, handle) <-
    liftIO $ openTempFileWithDefaultPermissions tmpDir filetpl
  liftIO (hPutStrLn handle header)
  count <- withSubmissions () (output handle)
  liftIO (hClose handle)
  return path
  where
    header = concat $ intersperse sep ["id", "user_id", "path", "language",
                                       "classify", "timing", "received"]
    output :: Handle -> () -> Submission -> IO ()
    output h _ Submission{..} = do
      let row = concat $ intersperse sep [show (fromSID id),
                                          show (fromUID userID),
                                          show path,
                                          show (fromLanguage $ codeLang code),
                                          show (resultClassify result),
                                          show timing,
                                          show (show received)
                                         ]
      hPutStrLn h row

                                      
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


