{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
{-
  Data types and methods for problems 
-}

module Problem ( 
  Problem(..),
  ProblemSet(..),
  readProblem,     -- * read a single problem
  readProblemSet,  -- * read a problem set
  isEarly,         -- * check problem's acceptance dates
  isLate,
  isOpen,          -- * can be submitted and accepted
  renderPandoc,   -- * render description into HTML
  Tagged, taglist, isTagged, hasTags  -- * problem tagging
  ) where

import           Control.Monad
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

import           Interval (Interval)
import qualified Interval as Interval
import           Types

import           ParseMeta

-- an individual problem
data Problem = Problem {
  probID     :: PID,              -- unique identifier
  probPath   :: FilePath,         -- relative filepath
  probTitle  :: Maybe Text,       -- title
  probDescr  :: Pandoc,           -- description 
  probTags   :: [Tag],            -- tag list 
  probOpen   :: Interval UTCTime, -- open interval
  probDoctest :: FilePath,        -- doctest file
  probDefault :: Maybe Text       -- default submission 
  } deriving Show


-- a problem set 
data ProblemSet = ProblemSet {
      probsetPath  :: FilePath   -- relative filepath
    , probsetTitle :: Maybe Text
    , probsetDescr :: Pandoc
    , probsetProbs :: [Problem] -- problems in listing order
    , probsetExam  :: Bool      -- is this an exam?
    , probsetPrintout :: Bool   -- should we produce a printout?
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


-- | lookup a tag in a meta key-value map and parse result
lookupFromMeta :: ParseMeta a => String -> Meta -> Maybe a
lookupFromMeta tag meta = lookupMeta tag meta >>= fromMeta


-- | low-level IO read functions
-- read a problemset from the file system
readProblemSet :: FilePath -> IO ProblemSet
readProblemSet filepath = 
  (readMarkdown myReaderOptions <$> readFile filepath) >>=  
  makeProblemSet filepath


makeProblemSet :: FilePath -> Pandoc -> IO ProblemSet
makeProblemSet filepath descr@(Pandoc meta blocks) = case optPaths of
  Nothing -> ioError $ userError "no problem list in meta data"
  Just paths -> do 
    probs <- mapM readProblem paths
    tz <- getCurrentTimeZone
    let int = localTimeToUTC tz <$> Interval.interval open close
    return ProblemSet { 
               probsetPath = filepath
             , probsetTitle = title
             , probsetDescr = descr
             , probsetProbs = map (override int) probs
             , probsetExam = exam
             , probsetPrintout = printout
             }
  where
    problemDir = takeDirectory filepath
    optPaths = map ((problemDir </>) . T.unpack) <$> lookupFromMeta "problems" meta 
    title = (lookupFromMeta "title" meta) `mplus` firstHeader blocks 
    open = lookupFromMeta "open" meta 
    close= lookupFromMeta "close" meta 
    exam  = maybe False id (lookupFromMeta "exam-mode" meta)
    printout = maybe False id (lookupFromMeta "printout" meta)

  


override :: Interval UTCTime -> Problem -> Problem
override time prob@Problem{..}
  = prob { probOpen = Interval.interval
                      (Interval.start probOpen `mplus` Interval.start time)
                      (Interval.end probOpen `mplus` Interval.end time) }


-- first header in a list of blocks
firstHeader :: [Block] -> Maybe Text
firstHeader blocks = listToMaybe [query inlineText h | Header _ _ h <- blocks]




readProblem :: FilePath -> IO Problem 
readProblem filepath =  do
      txt <- readFile filepath
      let ext = takeExtension filepath
      doc <- case lookup ext readersList of
            Just reader -> return (reader txt)
            Nothing -> ioError $ userError ("unknown extension " ++ show ext)
      makeProblem filepath doc

-- file extensions and associated Pandoc readers
readersList :: [(String, String -> Pandoc)]
readersList
  = [(ext, readMarkdown myReaderOptions) | ext<-[".md",".mdown",".markdown"]] ++
    [(ext, readHtml myReaderOptions)     | ext<-[".html", ".htm"]] ++
    [(".tex", readLaTeX myReaderOptions)]


-- make a problem from a Pandoc document
makeProblem :: FilePath ->  Pandoc -> IO Problem
makeProblem filepath descr@(Pandoc meta blocks)
    = do tz <- getCurrentTimeZone
         let int = localTimeToUTC tz <$> Interval.interval open close
         return Problem { probID = pid,
                          probPath = filepath,
                          probTitle = title,
                          probTags = tags,
                          probOpen = int,
                          probDoctest = doctest,
                          probDefault = submit,
                          probDescr = descr
                        }
  where
    -- take unique identifier from filepath
    pid = PID $ B.fromString $ takeBaseName filepath
    -- fetch metadata from Pandoc document
    open = lookupFromMeta "open" meta 
    close = lookupFromMeta "close" meta 
    title = (lookupFromMeta "title" meta) `mplus` firstHeader blocks
    submit = lookupFromMeta "submit" meta 
    tags =  maybe [] id (lookupFromMeta "tags" meta)
    doctest = case lookupFromMeta "doctest" meta of
      Just p -> takeDirectory filepath </> T.unpack p
      Nothing -> dropExtension filepath <.> "tst"



-- relations between problems and times
isEarly, isLate :: UTCTime -> Problem  -> Bool  
isEarly t Problem{..} = t `Interval.before` probOpen 
isLate t Problem{..} = t `Interval.after` probOpen 


-- check if a problem can be submited & accepted
isOpen :: UTCTime -> Problem  -> Bool
isOpen t Problem{..} = t `Interval.elem` probOpen


renderPandoc :: Pandoc -> [Node]
renderPandoc  = renderHtmlNodes . writeHtml myWriterOptions 


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

