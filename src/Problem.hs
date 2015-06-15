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

import           System.Locale (defaultTimeLocale)
import           Data.Time.Clock
import           Data.Time.Format
import           Data.Time.LocalTime
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

import           Interval (Interval)
import qualified Interval as Interval
import           Types



-- an individual problem
data Problem = Problem {
  probID     :: PID,              -- unique identifier (from filename)
  probTitle  :: Maybe Text,       -- title
  probDescr  :: Pandoc,           -- description 
  probTags   :: [Tag],            -- tag list 
  probOpen   :: Interval UTCTime, -- open interval
  probDoctest :: FilePath,        -- doctest file
  probDefault :: Maybe Text       -- default submission 
  } deriving Show


-- a problem set 
data ProblemSet = ProblemSet {
      probsetTitle :: Maybe Text
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




-- | low-level IO read functions
-- read a problemset from the file system
readProblemSet :: FilePath -> IO ProblemSet
readProblemSet filepath =
  (readMarkdown myReaderOptions <$> readFile filepath) >>=  
  makeProblemSet (takeDirectory filepath)


makeProblemSet :: FilePath -> Pandoc -> IO ProblemSet
makeProblemSet problemDir descr@(Pandoc meta blocks) = do
  tz <- getCurrentTimeZone
  -- open time interval for whole problemset 
  let time = localTimeToUTC tz <$> Interval.interval open close
  probs <- mapM readProblem paths
  return ProblemSet { probsetTitle = fetchTitle descr
                    , probsetDescr = descr
                    -- restrict each problem's availability interval 
                    , probsetProbs = map (restrict time) probs
                    , probsetExam = exam
                    , probsetPrintout = printout
                    }
  where
    restrict t p = p { probOpen = Interval.intersect t (probOpen p) }
    open  = fetchTime "open" meta
    close = fetchTime "close" meta
    exam  = maybe False id (fetchBool "exam-mode" meta)
    printout = maybe False id (fetchBool "printout" meta)
    paths = case lookupMeta "problems" meta of
      Just (MetaList l) -> map ((problemDir </>) . query inlineString) l
      _                 -> error "makeProblemSet: invalid problems list"
  



readProblem :: FilePath -> IO Problem 
readProblem filepath =  do
      txt <- readFile filepath
      let ext = takeExtension filepath
      let doc = case lookup ext extensionsList of
            Just reader -> reader myReaderOptions txt
            Nothing -> error ("readProblem: error reading " ++ show filepath)
      makeProblem filepath doc

-- file extensions and associated Pandoc readers
extensionsList :: [(String, ReaderOptions -> String -> Pandoc)]
extensionsList
  = [(ext, readMarkdown) | ext<-[".md",".mdown",".markdown"]] ++
    [(ext, readHtml)     | ext<-[".html", ".htm"]] ++
    [(".tex", readLaTeX)]



-- make a problem from a Pandoc document
makeProblem :: FilePath -> Pandoc -> IO Problem
makeProblem filepath descr@(Pandoc meta blocks)
    = do tz <- getCurrentTimeZone
         let time = localTimeToUTC tz <$> Interval.interval open close
         return Problem { probID = pid,
                          probTitle = fetchTitle descr,
                          probTags = tags,
                          probOpen = time,
                          probDoctest = doctest,
                          probDefault = submit,
                          probDescr = descr
                        }
  where
    -- assumes problem file basenames are unique
    pid = PID $ B.fromString $ takeBaseName filepath
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
fetchBool tag meta = lookupMeta tag meta >>= checkBool
  where checkBool (MetaBool b) = Just b
        checkBool _            = Nothing


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

