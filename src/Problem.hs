{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
{-
  Data types and methods for problems 
-}

module Problem ( 
  Problem(..),
  readProblem,     -- * get a single problem
  readProblemFile,
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
-- import           Control.Applicative ((<$>))
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


-- datatype for problems 
data Problem = Problem {
  probTitle  :: Maybe Text,       -- title
  probTags   :: [Text],           -- tag list 
  probOpen   :: Interval UTCTime, -- open interval
  probDoctest :: FilePath,        -- doctest file
  probDefault :: Maybe Text,      -- default submission 
  probDoc  :: Pandoc              -- description document
  } deriving Show

             

-- allowed file extensions and associated Pandoc readers
extensionsList :: [([String], ReaderOptions -> String -> Pandoc)]
extensionsList
  = [([".md",".mdown",".markdown"], readMarkdown),
     ([".htm",".html"], readHtml), ([".tex"], readLaTeX)]


-- read the problem directory;
-- return a list of all problem IDs in order
readProblemDir :: IO [PID]
readProblemDir = do
  list <- getDirectoryContents "problems"
  return $ sort $ map mkPID $ filter (accept.takeExtension) list
  where
    accept ext = ext `elem` concatMap fst extensionsList
    mkPID = PID . B.fromString 


readProblem :: PID -> IO Problem 
readProblem pid = readProblemFile ("problems" </> show pid)
    
-- read a markdown problem file
readProblemFile :: FilePath -> IO Problem
readProblemFile filepath = do
  txt <- readFile filepath
  let ext = takeExtension filepath
  let doc = head [reader myReaderOptions txt
                 | (exts, reader)<-extensionsList, ext`elem`exts]
  tz <- getCurrentTimeZone
  return (makeProblem tz filepath doc)



-- make a problem from a Pandoc document
makeProblem :: TimeZone -> FilePath -> Pandoc -> Problem
makeProblem tz filepath doc@(Pandoc meta blocks)
  = Problem { probTitle = fmap T.pack title,
              probTags = tags,
              probOpen = fmap (localTimeToUTC tz) $ parseInterval open close,
              probDoctest = doctest,
              probDefault = fmap T.pack submit,
              probDoc = doc
            }
  where
    -- fetch metadata from Pandoc document
    title = lookupInline "title" meta `mplus` lookupHeader blocks
    open = lookupInline "open" meta
    close = lookupInline "close" meta
    submit = lookupBlock "submit" meta
    tags = case lookupMeta "tags" meta of
      Just (MetaList l) -> map (T.pack . query inlineStrings) l
      _                 -> []
    doctest = case lookupInline "doctest" meta of
      Just file -> file
      Nothing -> dropExtension filepath <.> "tst" -- default test filepath 
    

-- lookup metadata as a string
lookupInline tag meta = do
  v <- lookupMeta tag meta
  return (query inlineStrings v)

lookupBlock tag meta = do
  v <- lookupMeta tag meta
  return (query blockStrings v)

-- lookup first header as a single string
lookupHeader blocks
  = do h <- listToMaybe [inlines | Header _ _ inlines <- blocks]
       return (query inlineStrings h)


-- collect strings 
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



-- parse strings as localtimes to make an interval
parseInterval :: Maybe String -> Maybe String -> Interval LocalTime
parseInterval open close
  =  Interval.interval (open >>= parseLocalTime) (close >>= parseLocalTime)
  
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
renderProblem  = renderHtmlNodes . writeHtml myWriterOptions . probDoc


--- pandoc reader and writter options
myReaderOptions :: ReaderOptions
myReaderOptions = def { readerExtensions = pandocExtensions }

myWriterOptions :: WriterOptions
myWriterOptions = def { writerHTMLMathMethod = MathJax "/mathjax",
                        writerHighlight = True
                      }
