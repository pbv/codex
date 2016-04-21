{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-
  Data types and methods for problem and worksheet pages
-}
module Problem  where

import           Control.Monad
import           Control.Applicative 

import           Data.Maybe
import           Data.Monoid
import           Data.String
import           Data.Text(Text)
import qualified Data.Text             as T

import           Data.Time.LocalTime
import           Data.Time.Format
import           Data.Time.Clock
import           System.Locale (defaultTimeLocale)

import           Language.Types
import           Markdown
import           Text.Pandoc hiding (Code)
import           Text.Pandoc.Walk
import           Types

import           System.FilePath
import           System.Directory


-- | a page: either a single problem or a worksheet
data Page
  = Page { root :: FilePath        -- root dir for this page
         , path :: FilePath        -- relative path
         , meta :: Meta
         , description :: [Block]
         , contents :: Contents  
         } deriving Show

data Contents
  = Problem                -- terminal page
  | Worksheet [FilePath]   -- relative paths to linked pages
  deriving Show
                         

-- | fetch page title
getTitle :: Page -> Text
getTitle Page{..}
  = fromMaybe (T.pack path) 
    (lookupFromMeta "title" meta <|>  firstHeader description )

  
-- help function;
-- get first header frm a list of blocks
firstHeader :: [Block] -> Maybe Text
firstHeader blocks = listToMaybe [query inlineText h | Header _ _ h <- blocks]

-- | fetch page tags
getTags :: Page -> [Text]
getTags Page{..} = fromMaybe [] (lookupFromMeta "tags" meta)

getLanguage :: Page -> Maybe Language
getLanguage Page{..} = lookupFromMeta "language" meta

getCodeText :: Page -> Maybe Text
getCodeText Page{..} = lookupFromMeta "code" meta

getCode :: Page -> Maybe Code
getCode page = Code (getLanguage page) <$> getCodeText page



-- parse time strings
parseLocalTime :: String -> Maybe LocalTime
parseLocalTime txt 
  = msum [parseTime defaultTimeLocale fmt txt | fmt<-timeFormats] 
  where timeFormats = ["%H:%M %d/%m/%Y", "%d/%m/%Y", "%c"]

parseUTCTime :: TimeZone -> String -> Maybe UTCTime
parseUTCTime tz txt = localTimeToUTC tz <$> parseLocalTime txt


-- | read a page from a markdown file
readPage :: FilePath -> FilePath -> IO Page
readPage root path = do
  let filepath = root </> path
  Pandoc meta blocks <- readMarkdownFile filepath
  case lookupFromMeta "contents" meta of
    Just paths -> do
      -- interpret paths relative to the current page 
      let dir = takeDirectory path
      let paths' = map (normalise . (dir</>)) paths
      return Page { root = root
                  , path = path
                  , meta = meta
                  , description = blocks
                  , contents = Worksheet paths'
                  }
    Nothing -> do
        -- tz <- getCurrentTimeZone  
        -- let time = lookupFromMeta "limit" meta >>= parseUTCTime tz
        return Page { root = root
                    , path = path
                    , meta = meta
                    , description = blocks
                    , contents = Problem 
                    }






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


