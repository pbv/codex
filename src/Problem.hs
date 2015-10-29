{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
{-
  Data types and methods for problems 
-}

module Problem ( 
  Problem(..),
  ProblemSet(..),
  -- readProblem,     -- * read a single problem
  readProblemSet,  -- * read a problem set
  --isEarly,         -- * check problem's acceptance dates
  --isLate,
  isOpen,          -- * can be submitted and accepted
  renderPandoc,   -- * render description into HTML
  Tagged, taglist, isTagged, hasTags  -- * problem tagging
  ) where

import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Applicative ((<$>))
import           System.FilePath

import qualified Data.ByteString.UTF8 as B

import           Data.Text(Text)
import qualified Data.Text             as T
import           Data.Maybe (listToMaybe)

import qualified Data.Set as Set

import           Text.Pandoc
import           Text.Pandoc.Walk
import           Text.XmlHtml 
import           Text.Blaze.Renderer.XmlHtml

import           Data.Time.LocalTime
import           Data.Time.Clock

import           Types

import           ParseMeta
import           LogIO
import           System.Directory(doesFileExist)


-- individual problems
data Problem = Problem {
  probID       :: PID,             -- unique identifier
  probPath     :: FilePath,        -- relative filepath
  probTitle    :: Maybe Text,      -- title
  probDescr    :: Pandoc,          -- description 
  probTags     :: [Tag],           -- tag list 
  probDeadline :: Maybe UTCTime,   -- optional acceptance deadline
  probDoctest  :: FilePath,        -- doctest file
  probDefault  :: Maybe Text,       -- default submission
  probVisible  :: Bool              -- visible?
  } deriving Show


-- a problem set 
data ProblemSet = ProblemSet {
      probsetPath  :: FilePath   -- relative filepath
    , probsetTitle :: Maybe Text
    , probsetDescr :: Pandoc
    , probsetProbs :: [Problem] -- problem list 
    , probsetExam  :: Bool      -- is this an exam?
    , probsetPrintout :: Bool   -- should we printouts?
    } deriving Show


-- | collect all tags from problems and problem sets
instance Tagged Problem where
    taglist = probTags

instance Tagged a => Tagged [a] where
    taglist l = Set.toList $ Set.fromList $ concatMap taglist l

instance Tagged ProblemSet where
    taglist ProblemSet{..} = dynamic ++ taglist probsetProbs
      where  dynamic = ["*accepted*", "*not accepted*", 
                        "*submitted*", "*not submitted*"]




lookupFromMeta :: ParseMeta a => String -> Meta -> LogIO (Maybe a)
lookupFromMeta tag meta = case lookupMeta tag meta of
  Nothing -> return Nothing
  Just v -> case parseMeta v of
    Left err -> logString ("metadata " ++ show tag ++ ": " ++ err) >>
                return Nothing
    Right r -> return (Just r)
    
                   
lookupUTC :: TimeZone -> String -> Meta -> LogIO (Maybe UTCTime)
lookupUTC tz tag meta = fmap (localTimeToUTC tz) <$> lookupFromMeta tag meta


-- | read a problemset from the file system
-- also yields list of warningtext messages 
readProblemSet :: FilePath -> IO (ProblemSet, [Text])
readProblemSet filepath = runLogIO $ do
  doc <- readMarkdown myReaderOptions <$> safeIO "" (readFile filepath) 
  logPrefix filepath (makeProblemSet filepath doc)


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

-- first header in a list of blocks
firstHeader :: [Block] -> Maybe Text
firstHeader blocks = listToMaybe [query inlineText h | Header _ _ h <- blocks]


readProblem :: FilePath -> LogIO Problem 
readProblem filepath = logPrefix filepath $ do
      txt <- safeIO "" (readFile filepath)
      doc <- case lookup (takeExtension filepath) readersList of
            Just reader -> return (reader txt)
            Nothing -> do logString "invalid file extension"
                          return (Pandoc nullMeta [])
      makeProblem filepath doc



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


{-
-- relations between problems and times
isEarly, isLate :: UTCTime -> Problem  -> Bool  
isEarly t Problem{..} = t `Interval.before` probOpen 
isLate t Problem{..} = t `Interval.after` probOpen 
-}

-- check if a problem can be submited & accepted
isOpen :: UTCTime -> Problem -> Bool
isOpen t Problem{..} = case probDeadline of Just deadline -> t<=deadline
                                            Nothing -> True




-- | render a Pandoc document into a list of HTML nodes
renderPandoc :: Pandoc -> [Node]
renderPandoc  = renderHtmlNodes . writeHtml myWriterOptions 


-- file extensions and associated Pandoc readers
readersList :: [(String, String -> Pandoc)]
readersList
  = [(ext, readMarkdown myReaderOptions) | ext<-[".md",".mdown",".markdown"]] ++
    [(ext, readHtml myReaderOptions)     | ext<-[".html", ".htm"]] ++
    [(".tex", readLaTeX myReaderOptions)]


--- pandoc reader and writter options
myReaderOptions :: ReaderOptions
myReaderOptions = def { readerExtensions = pandocExtensions
                      , readerSmart = True 
                      }

myWriterOptions :: WriterOptions
myWriterOptions = def { writerExtensions = pandocExtensions
                      , writerHTMLMathMethod = MathJax "/mathjax",
                        writerHighlight = True
                      }

