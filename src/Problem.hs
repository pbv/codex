{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
{-
  Data types and methods for problems 
-}

module Problem ( 
  Problem(..),
  ProblemSet(..),
  -- readProblem,     -- * read a single problem
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
import           Control.Monad.Writer
import           Control.Monad.Reader
import           System.IO.Error
import           System.Directory


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


-- | monad combining IO with logging
-- reader enviromemnt for logging context             
type LogIO a = ReaderT Text (WriterT [Text] IO) a

runLogIO :: LogIO a -> IO (a, [Text])
runLogIO m = runWriterT (runReaderT m T.empty)

-- | log a string
logStr :: String -> LogIO ()
logStr s = do r <- ask
              tell [T.append r (T.pack $ s)]

context :: String -> LogIO a -> LogIO a
context s = local (const (T.pack $ s ++ ": ")) 


-- | safe liftIO that logs IO error and returns a default value
safeIO :: a -> IO a -> LogIO a
safeIO def m = do
  r <- liftIO (catchIOError (Right <$> m) (return . Left))
  case r of
    Left err -> tell [T.pack (show err)] >> return def
    Right v -> return v


{-
-- | lookup a tag in a meta key-value map and parse result
lookupFromMeta :: ParseMeta a => String -> Meta -> Maybe a
lookupFromMeta tag meta = lookupMeta tag meta >>= fromMeta
-}


lookupFromMeta :: ParseMeta a => String -> Meta -> LogIO (Maybe a)
lookupFromMeta tag meta = case lookupMeta tag meta of
  Nothing -> return Nothing
  Just v -> case parseMeta v of
    Left err -> logStr ("metadata " ++ show tag ++ ": " ++ err) >>
                return Nothing
    Right r -> return (Just r)
    
                   
lookupUTC :: TimeZone -> String -> Meta -> LogIO (Maybe UTCTime)
lookupUTC tz tag meta = fmap (localTimeToUTC tz) <$> lookupFromMeta tag meta


-- | read a problemset from the file system
-- also yields list of warningtext messages 
readProblemSet :: FilePath -> IO (ProblemSet, [Text])
readProblemSet filepath = runLogIO $ do
  doc <- readMarkdown myReaderOptions <$> safeIO "" (readFile filepath) 
  context filepath (makeProblemSet filepath doc)


makeProblemSet :: FilePath -> Pandoc -> LogIO ProblemSet
makeProblemSet filepath descr@(Pandoc meta blocks) = do
  tz <- liftIO getCurrentTimeZone
  optPaths <- lookupFromMeta "problems" meta
  -- fmap (map T.unpack) <$> lookupFromMeta "problems" meta
  let paths = maybe [] (map (problemDir </>)) optPaths
  title <- lookupFromMeta "title" meta
  open <- lookupUTC tz "open" meta
  close <- lookupUTC tz "close" meta
  exam <- maybe False id <$> lookupFromMeta "exam-mode" meta
  printout <- maybe False id <$> lookupFromMeta "printout" meta
  probs <- mapM readProblem paths
  return ProblemSet { 
    probsetPath = filepath
    , probsetTitle = title `mplus` firstHeader blocks
    , probsetDescr = descr
    , probsetProbs = map (override open close) probs
    , probsetExam = exam
    , probsetPrintout = printout
    }
  where
    problemDir = takeDirectory filepath


override :: Maybe UTCTime -> Maybe UTCTime -> Problem -> Problem
override open close prob@Problem{..}
  = prob { probOpen = Interval.interval
                      (Interval.start probOpen `mplus` open)
                      (Interval.end probOpen `mplus` close) }


-- first header in a list of blocks
firstHeader :: [Block] -> Maybe Text
firstHeader blocks = listToMaybe [query inlineText h | Header _ _ h <- blocks]


readProblem :: FilePath -> LogIO Problem 
readProblem filepath = context filepath $ do
      txt <- safeIO "" (readFile filepath)
      doc <- case lookup (takeExtension filepath) readersList of
            Just reader -> return (reader txt)
            Nothing -> do logStr "invalid file extension"
                          return (Pandoc nullMeta [])
      makeProblem filepath doc



-- file extensions and associated Pandoc readers
readersList :: [(String, String -> Pandoc)]
readersList
  = [(ext, readMarkdown myReaderOptions) | ext<-[".md",".mdown",".markdown"]] ++
    [(ext, readHtml myReaderOptions)     | ext<-[".html", ".htm"]] ++
    [(".tex", readLaTeX myReaderOptions)]


-- make a problem from a Pandoc document
makeProblem :: FilePath ->  Pandoc -> LogIO Problem
makeProblem filepath descr@(Pandoc meta blocks) = do
  tz <- liftIO getCurrentTimeZone
  open <-  lookupUTC tz "open" meta
  close <- lookupUTC tz "close" meta
  title <- lookupFromMeta "title" meta
  submit <- lookupFromMeta "submit" meta
  tags <- maybe [] id <$> lookupFromMeta "tags" meta
  optDoctest <- fmap T.unpack <$> lookupFromMeta "doctest" meta
  let doctest = maybe defaultDoctest (dir</>) optDoctest
  check <- liftIO (doesFileExist doctest)
  when (not check) $
    logStr ("doctest file " ++ show doctest ++ " does not exist")
  return Problem { probID = pid,
                   probPath = filepath,
                   probTitle = title `mplus` firstHeader blocks,
                   probTags = tags,
                   probOpen = Interval.interval open close,
                   probDoctest = doctest,
                   probDefault = submit,
                   probDescr = descr
                 }
  where
    -- take unique identifier from filepath
    pid = PID $ B.fromString $ takeBaseName filepath
    dir = takeDirectory filepath
    defaultDoctest = dropExtension filepath <.> "tst"



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

