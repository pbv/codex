{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-
  Data types and methods for problems
-}

module Problem  where

import           Control.Monad
-- import           Control.Monad.State
-- import           Control.Monad.Trans
import           Control.Applicative 

-- import qualified Data.ByteString.UTF8 as B

import           Data.Maybe
import           Data.Monoid
import           Data.String
import           Data.Text(Text)
import qualified Data.Text             as T

---import qualified Data.HashSet as Set
-- import           Data.HashSet (HashSet)


import           Data.Time.LocalTime
import           Data.Time.Format
import           Data.Time.Clock
import           System.Locale (defaultTimeLocale)

-- import           Data.Aeson (encode)
-- import           Snap.Snaplet.SqliteSimple

import           Language.Types
import           Markdown
-- import           FromMeta
import           Text.Pandoc hiding (Code)
import           Text.Pandoc.Walk
-- import           Text.Pandoc.Builder hiding (Code)
-- import           Data.Monoid
-- import           Text.XmlHtml 
-- import           Text.Blaze.Renderer.XmlHtml
-- import           Application
import           Types

-- import           Language
-- import           Tester
-- import           ParseMeta
-- import           LogIO
import           System.FilePath
import           System.Directory


-- | a page: either a single problem or a worksheet
data Page
  = Worksheet { path :: FilePath
              , meta :: Meta
              , description :: [Block]
              , contents :: [Page]
              }
  | Problem { path :: FilePath
            , meta :: Meta
            , description :: [Block]
            , limit :: Maybe UTCTime
            } deriving Show



isProblem, isWorksheet :: Page -> Bool
isProblem Problem{..} = True
isProblem _           = False

isWorksheet Worksheet{..} = True
isWorksheet _             = False

-- | fetch page title
getTitle :: Page -> Text
getTitle page
  = fromMaybe (T.pack $ path page) 
      (lookupFromMeta "title" (meta page) <|>
       firstHeader (description page))

  
-- help function;
-- get first header frm a list of blocks
firstHeader :: [Block] -> Maybe Text
firstHeader blocks = listToMaybe [query inlineText h | Header _ _ h <- blocks]

-- | fetch page tags
getTags :: Page -> [Text]
getTags page = fromMaybe [] (lookupFromMeta "tags" (meta page))

getLanguage :: Page -> Maybe Language
getLanguage page = lookupFromMeta "language" (meta page)


getCodeText :: Page -> Maybe Text
getCodeText page = lookupFromMeta "code" (meta page)

getCode :: Page -> Maybe Code
getCode page = do
  lang <- getLanguage page
  txt <- getCodeText page
  return (Code lang txt)


-- parse time strings
parseLocalTime :: String -> Maybe LocalTime
parseLocalTime txt 
  = msum [parseTime defaultTimeLocale fmt txt | fmt<-timeFormats] 
  where timeFormats = ["%H:%M %d/%m/%Y", "%d/%m/%Y", "%c"]

parseUTCTime :: TimeZone -> String -> Maybe UTCTime
parseUTCTime tz txt = localTimeToUTC tz <$> parseLocalTime txt


readPage :: FilePath -> IO Page
readPage filepath = do
  tz <- getCurrentTimeZone
  readPageAux tz maxDepth mempty filepath
  where maxDepth = 4

readPageAux :: TimeZone -> Int -> Meta -> FilePath -> IO Page
readPageAux tz n m filepath = do
  Pandoc meta blocks <- readMarkdownFile filepath
  let m' = meta <> m
  case lookupFromMeta "contents" meta of
    Just paths -> do
      pages <- readPaths m' paths
      return Worksheet { path = normalise filepath
                       , meta = m'
                       , description = blocks
                       , contents =  pages
                       }
    Nothing -> do
      let t = lookupFromMeta "limit" meta >>= parseUTCTime tz
      return Problem { path = normalise filepath
                     , meta = m'
                     , description = blocks
                     , limit = t
                     }
  where
    readPaths m' paths | n>0 = do
      let dir = takeDirectory filepath
      mapM (\path -> readPageAux tz (n-1) m' (dir</>path)) paths
    readPaths _ _  = return []






{-
-- | read a worksheet document and fetches problems;
-- appends the worksheet metadata to each problem
readWorksheet :: FilePath -> IO Worksheet
readWorksheet filepath = do
  let dir = takeDirectory filepath   -- get the worksheet directory
  Pandoc meta blocks <- readMarkdownFile filepath -- read Pandoc document
  let paths =  fromMaybe [] $ lookupFromMeta "problems" meta  -- fetch paths
  probs <- mapM readProblem (map (dir</>) paths)  -- read each problem
  return Worksheet { wsPath = normalise filepath
                   , wsMeta = meta
                   , wsDescription = blocks
                   , wsProblems = map (appendMeta meta) probs
                   }

-}



{-             
lookupUTC :: TimeZone -> String -> Meta -> LogIO (Maybe UTCTime)
lookupUTC tz tag meta = fmap (localTimeToUTC tz) <$> lookupFromMeta tag meta
-}

{-
-- | read a problemset from the file system
-- also yields list of warningtext messages 
readProblemSet :: FilePath -> IO (ProblemSet, [Text])
readProblemSet filepath = runLogIO $ do
  doc <- readMarkdown myReaderOptions <$> safeIO "" (readFile filepath) 
  logPrefix filepath (makeProblemSet filepath doc)
-}

{-
makeProblemSet :: FilePath -> Pandoc -> LogIO ProblemSet
makeProblemSet filepath descr@(Pandoc meta blocks) = do
  tz <- liftIO getCurrentTimeZone
  optPaths <- lookupFromMeta "problems" meta
  paths <- case optPaths of 
    Nothing -> logString "error: could not find problem list" >> return []
    Just ps -> return (map (problemDir </>) ps)
  title <- lookupFromMeta "title" meta
  exam <- maybe False id <$> lookupFromMeta "exam-mode" meta
  printout <- maybe False id <$> lookupFromMeta "printout" meta
  deadline <- lookupUTC tz "deadline" meta
  let override p = p { probDeadline = probDeadline p `mplus` deadline }
  probs <- mapM readProblem paths
  return ProblemSet { 
    probsetPath = filepath
    , probsetTitle = title `mplus` firstHeader blocks
    , probsetDescr = descr
    , probsetProbs = map override probs
    , probsetExam = exam
    , probsetPrintout = printout
    }
  where
    problemDir = takeDirectory filepath
-}

{-
-- first header in a list of blocks
firstHeader :: [Block] -> Maybe Text
firstHeader blocks = listToMaybe [query inlineText h | Header _ _ h <- blocks]
-}

{-
readProblem :: FilePath -> LogIO Problem 
readProblem filepath = logPrefix filepath $ do
      txt <- safeIO "" (readFile filepath)
      doc <- case lookup (takeExtension filepath) readersList of
            Just reader -> return (reader txt)
            Nothing -> do logString "invalid file extension"
                          return (Pandoc nullMeta [])
      makeProblem filepath doc
-}

{-
-- make a problem from a Pandoc document
makeProblem :: FilePath ->  Pandoc -> LogIO Problem
makeProblem filepath descr@(Pandoc meta blocks) = do
  tz <- liftIO getCurrentTimeZone
  title <- lookupFromMeta "title" meta
  submit <- lookupFromMeta "submit" meta
  tags <- maybe [] id <$> lookupFromMeta "tags" meta
  deadline <-  lookupUTC tz "deadline" meta
  visible <- maybe True id <$> lookupFromMeta "visible" meta
  optDoctest <- lookupFromMeta "doctest" meta
  let doctest = maybe defaultDoctest (dir</>) optDoctest
  check <- liftIO (doesFileExist doctest)
  when (not check) $
    logString ("doctest file " ++ show doctest ++ " does not exist")
  return Problem { probID = pid,
                   probPath = filepath,
                   probTitle = title `mplus` firstHeader blocks,
                   probTags = tags,
                   probDeadline = deadline,
                   probDoctest = doctest,
                   probDefault = submit,
                   probDescr = descr,
                   probVisible = visible
                 }
  where
    -- take unique identifier from filepath
    pid = PID $ B.fromString $ takeBaseName filepath
    dir = takeDirectory filepath
    defaultDoctest = dropExtension filepath <.> "tst"
-}

{-
-- relations between problems and times
isEarly, isLate :: UTCTime -> Problem  -> Bool  
isEarly t Problem{..} = t `Interval.before` probOpen 
isLate t Problem{..} = t `Interval.after` probOpen 
-}

-- check if a problem can be submited and accepted
--isAcceptable :: UTCTime -> Problem -> Bool
--isAcceptable t Problem{..} = maybe True (t<=) probLimit


