{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
{-
  Data types and methods for problems 
-}

module Problem ( 
  Problem(..),
  ProblemSet(..),
  readProblem,     -- * read a single problem
  readProblemSet,  -- * read a problem set
  readProblemDir,  -- * list all problem ids
  isEarly,         -- * check problem's acceptance dates
  isLate,
  isOpen,          -- * can be submitted and accepted
  renderProblem    -- * render problem description into HTML
  ) where

-- import           Prelude hiding(catch)
import           Data.List (sort)
import           System.Locale (defaultTimeLocale)
import           Data.Time.Clock
import           Data.Time.Format
import           Data.Time.LocalTime
import           Control.Monad
import           Control.Applicative ((<$>))
import           System.FilePath
import           System.Directory

-- import           Data.ByteString.UTF8(ByteString)
import qualified Data.ByteString.UTF8 as B

import           Data.Text(Text)
import qualified Data.Text             as T
import           Data.Maybe (listToMaybe)

import           Interval (Interval)
import qualified Interval as Interval
import           Types 

import           Text.Pandoc
import           Text.Pandoc.Walk
import           Text.XmlHtml 
import           Text.Blaze.Renderer.XmlHtml


-- a problem set 
data ProblemSet = ProblemSet {
      probsetTitle :: Maybe Text
    , probsetDescr :: Pandoc
    --, probsetOpen  :: Interval UTCTime
    , probsetProbs :: [Problem] -- problems in listing order
    , probsetExam  :: Bool
    , probsetPrintout :: Bool
    } deriving Show


-- an individual problem
data Problem = Problem {
  probID     :: PID,              -- unique identifier (from filename)
  probTitle  :: Maybe Text,       -- title
  probDescr  :: Pandoc,           -- description 
  probTags   :: [ProblemTag],     -- tag list 
  probOpen   :: Interval UTCTime, -- open interval
  probDoctest :: FilePath,        -- doctest file
  probDefault :: Maybe Text       -- default submission 
  } deriving Show



{-
-- check if a problem can be submited
problemAvailable :: UTCTime -> ProblemSet -> Problem -> Bool
problemAvailable now ProblemSet{..} Problem{..}
    = probID `elem` probsetIDs &&
      (not probsetExam || 
       now `Interval.elem` (Interval.intersect probsetOpen probOpen))
-}


readProblemSet :: FilePath -> IO ProblemSet
readProblemSet filepath = 
  (readMarkdown myReaderOptions <$> readFile filepath) >>=  makeProblemSet 

makeProblemSet descr@(Pandoc meta blocks) = do
  tz <- getCurrentTimeZone
  -- open time interval for whole problemset 
  let int = localTimeToUTC tz <$> Interval.interval open close
  probs <- mapM readProblem pids 
  return ProblemSet { probsetTitle = fetchTitle descr
                    , probsetDescr = descr
                    -- , probsetOpen = int
                    -- restrict each problem's availability interval 
                    , probsetProbs = map (restrict int) probs
                    , probsetExam = exam
                    , probsetPrintout = printout
                    }
  where
    restrict int p = p { probOpen = Interval.intersect int (probOpen p) }
    -- lookup metadata 
    open = fetchTime "open" meta
    close = fetchTime "close" meta
    exam = maybe False id (fetchBool "exam-mode" meta)
    printout = maybe False id (fetchBool "printouts" meta)
    pids = case lookupMeta "problems" meta of
      Just (MetaList l) -> map (PID . query inlineByteString) l
      _                 -> []
  

-- read the problem directory;
-- return a list of all problem IDs in order
readProblemDir :: IO [PID]
readProblemDir = do
  list <- getDirectoryContents problemDir
  return $ sort $ map mkPID $ filter (accept.takeExtension) list
  where
    accept ext = ext `elem` concatMap fst extensionsList
    mkPID = PID . B.fromString 


readProblem :: PID -> IO Problem 
readProblem pid =
  let filepath = problemDir </> show pid
  in do
  txt <- readFile filepath
  let ext = takeExtension filepath
  let doc = head [reader myReaderOptions txt
                 | (exts, reader)<-extensionsList, ext`elem`exts]
  makeProblem pid filepath doc

-- allowed problem file extensions and associated Pandoc readers
extensionsList :: [([String], ReaderOptions -> String -> Pandoc)]
extensionsList
  = [([".md",".mdown",".markdown"], readMarkdown),
     ([".htm",".html"],             readHtml), 
     ([".tex"],                     readLaTeX)]



-- make a problem from a Pandoc document
makeProblem :: PID -> FilePath -> Pandoc -> IO Problem
makeProblem pid filepath descr@(Pandoc meta blocks)
    = do tz <- getCurrentTimeZone
         let int = localTimeToUTC tz <$> Interval.interval open close
         return Problem { probID = pid,
                          probTitle = fetchTitle descr,
                          probTags = tags,
                          probOpen = int,
                          probDoctest = doctest,
                          probDefault = submit,
                          probDescr = descr
                        }
  where
    -- fetch metadata from Pandoc document
    open = fetchTime "open" meta
    close = fetchTime "close" meta
    submit = query blockText <$> lookupMeta "submit" meta
    tags = case lookupMeta "tags" meta of
      Just (MetaList l) -> map (query inlineText) l
      _                 -> []
    doctest = case lookupMeta "doctest" meta of
      Just l -> takeDirectory filepath </> query inlineString l
      Nothing -> dropExtension filepath <.> "tst" -- default test filepath 



-- lookup Pandoc metadata an parse results into Haskell datatypes
fetchTitle :: Pandoc -> Maybe Text
fetchTitle (Pandoc meta blocks) 
    = fmap (query inlineText) (lookupMeta "title" meta) 
      `mplus` 
      fmap (query blockText) firstHeader
    where firstHeader = listToMaybe [h | Header _ _ h <- blocks]

fetchTime :: String -> Meta -> Maybe LocalTime
fetchTime tag meta 
    = (query inlineString <$> lookupMeta tag meta) >>= parseLocalTime


fetchBool :: String -> Meta -> Maybe Bool
fetchBool tag meta = do
  s <- query inlineString <$> lookupMeta tag meta 
  case reads s of
    (b, ""):_ -> Just b
    _         -> Nothing
        

-- from Text.Pandoc.Definition
-- lookupMeta :: String -> Meta -> Maybe MetaValue 
{-
fetchText :: String -> Meta -> Maybe Text
fetchText tag meta = query inlineText <$> lookup tag meta

fetchString :: String -> Meta -> Maybe String
fetchString tag meta = query inlineString <$> lookup tag meta


lookupInline tag meta = do
  v <- lookupMeta tag meta
  return (query inlineStrings v)

lookupBlock tag meta = do
  v <- lookupMeta tag meta
  return (query blockStrings v)
-}




-- collect text in inline and block elements
inlineText :: Inline -> Text
inlineText (Str s)   = T.pack s
inlineText Space     = T.singleton ' '
inlineText LineBreak = T.singleton '\n'
inlineText (Math _ s)= T.pack s
inlineText _         = T.empty


blockText :: Block -> Text
blockText (Plain l)       = query inlineText l
blockText (Para p)        = query inlineText p
blockText (Header _ _ l)  = query inlineText l
blockText (CodeBlock _ s) = T.pack s
blockText (RawBlock  _ s) = T.pack s
blockText _               = T.empty


inlineString = T.unpack . inlineText
inlineByteString = B.fromString . inlineString
-- blockString  = T.unpack . blockText



{-
inlineStrings :: Inline -> String
inlineStrings (Str s) = s
inlineStrings Space   = " "
inlineStrings LineBreak = "\n"
inlineStrings _ = ""

blockStrings :: Block -> String
blockStrings (Plain l) =  query inlineStrings l
blockStrings (Para p)  = query inlineStrings p
blockStrings (CodeBlock _ s)= s
blockStrings (RawBlock _ s)= s
blockStrings _ = ""
-}

{-
-- parse strings as localtimes to make an interval
parseInterval :: Maybe String -> Maybe String -> Interval LocalTime
parseInterval open close
  =  Interval.interval (open >>= parseLocalTime) (close >>= parseLocalTime)
-}
  
-- parse a local time string 
parseLocalTime :: String -> Maybe LocalTime
parseLocalTime txt = msum [parseTime defaultTimeLocale fmt txt | fmt<-timeFormats] 

timeFormats :: [String]
timeFormats = ["%H:%M %d/%m/%Y", "%d/%m/%Y", "%c"]


-- relations between problems and times
isEarly, isLate :: UTCTime -> Problem  -> Bool  
isEarly t Problem{..} = t `Interval.before` probOpen 
isLate t Problem{..} = t `Interval.after` probOpen 


-- check if a problem can be submited & accepted
isOpen :: UTCTime -> Problem  -> Bool
isOpen t Problem{..} = t `Interval.elem` probOpen


-- render a problem into XHTML nodes
renderProblem :: Problem -> [Node]
renderProblem  = renderHtmlNodes . writeHtml myWriterOptions . probDescr


--- pandoc reader and writter options
myReaderOptions :: ReaderOptions
myReaderOptions = def { readerExtensions = pandocExtensions }

myWriterOptions :: WriterOptions
myWriterOptions = def { writerHTMLMathMethod = MathJax "/mathjax",
                        writerHighlight = True
                      }


-- problem directory path 
problemDir :: FilePath
problemDir = "problems"

