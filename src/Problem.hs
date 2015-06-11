{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
{-
  Data types and methods for problems 
-}

module Problem ( 
  Problem(..),
  ProblemSet(..),
  -- getProblemSet,   -- * query the current problemset
  -- getProblem,
  -- lookupProblemSet,
  readProblem,     -- * read a single problem
  readProblemSet,  -- * read a problem set
  -- readProblemDir,  -- * list all problem ids
  isEarly,         -- * check problem's acceptance dates
  isLate,
  isOpen,          -- * can be submitted and accepted
  -- renderProblem,    -- * render problem description into HTML
  renderPandoc,
  Tagged, taglist, isTagged, hasTags  -- * problem tagging
  ) where

-- import           Prelude hiding(catch)
import           Data.List (sort)
import           System.Locale (defaultTimeLocale)
import           Data.Time.Clock
import           Data.Time.Format
import           Data.Time.LocalTime
import           Control.Monad
import           Control.Monad.Trans (liftIO)
import           Control.Applicative ((<$>))
import           System.FilePath
import           System.Directory

-- import           Data.ByteString.UTF8(ByteString)
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
import           Application


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




-- a type class for collecting tags from problems, etc.
class Tagged a where
    taglist  :: a -> [Tag]

instance Tagged Problem where
    taglist = probTags

instance Tagged a => Tagged [a] where
    taglist l = Set.toList $ Set.fromList $ concatMap taglist l

instance Tagged ProblemSet where
    taglist ProblemSet{..} = dynamic ++ taglist probsetProbs
      where  dynamic = ["*accepted*", "*not accepted*", 
                        "*submitted*", "*not submitted*"]

-- check if something is tagged with a single tag
isTagged :: Tagged a => Tag -> a -> Bool
isTagged tag a = tag `elem` taglist a

-- check if something is tagged with a list of tags
hasTags :: Tagged a => [Tag] -> a -> Bool
hasTags tags a = tags `isSublistOf` taglist a

-- sublist checking
isSublistOf :: Eq a => [a] -> [a] -> Bool
isSublistOf xs ys = all (`elem`ys) xs


{-
-- check if a problem can be submited
problemAvailable :: UTCTime -> ProblemSet -> Problem -> Bool
problemAvailable now ProblemSet{..} Problem{..}
    = probID `elem` probsetIDs &&
      (not probsetExam || 
       now `Interval.elem` (Interval.intersect probsetOpen probOpen))
-}





-- read a problemset from the file system
readProblemSet :: IO ProblemSet
readProblemSet =
  (readMarkdown myReaderOptions <$> readFile problemSetPath) >>=
  makeProblemSet

makeProblemSet descr@(Pandoc meta blocks) = do
  tz <- getCurrentTimeZone
  -- open time interval for whole problemset 
  let time = localTimeToUTC tz <$> Interval.interval open close
  probs <- mapM readProblem pids
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
    pids = case lookupMeta "problems" meta of
      Just (MetaList l) -> map (PID . query inlineByteString) l
      _                 -> []
  

{-
-- read the problem directory;
-- return a list of all problem IDs in order
readProblemDir :: IO [PID]
readProblemDir = do
  list <- getDirectoryContents problemDirPath
  return $ sort $ map mkPID $ filter (accept.takeExtension) list
  where
    accept ext = ext `elem` concatMap fst extensionsList
    mkPID = PID . B.fromString 
-}


readProblem :: PID -> IO Problem 
readProblem pid 
  = let filepath = problemDirPath </> show pid
    in do
      txt <- readFile filepath
      let ext = takeExtension filepath
      let doc = case lookup ext extensionsList of
            Just reader -> reader myReaderOptions txt
            Nothing -> error ("readProblem: invalid file " ++ show pid)
      makeProblem pid filepath doc

-- file extensions and associated Pandoc readers
extensionsList :: [(String, ReaderOptions -> String -> Pandoc)]
extensionsList
  = [(ext, readMarkdown) | ext<-[".md",".mdown",".markdown"]] ++
    [(ext, readHtml)     | ext<-[".html", ".htm"]] ++
    [(".tex", readLaTeX)]



-- make a problem from a Pandoc document
makeProblem :: PID -> FilePath -> Pandoc -> IO Problem
makeProblem pid filepath descr@(Pandoc meta blocks)
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


fetchText :: String -> Meta -> Maybe Text
fetchText tag meta = query inlineText <$> lookupMeta tag meta



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
-- renderProblem :: Problem -> [Node]
-- renderProblem  = renderHtmlNodes . writeHtml myWriterOptions . probDescr


renderPandoc :: Pandoc -> [Node]
renderPandoc  = renderHtmlNodes . writeHtml myWriterOptions 


--- pandoc reader and writter options
myReaderOptions :: ReaderOptions
myReaderOptions = def { readerExtensions = pandocExtensions }

myWriterOptions :: WriterOptions
myWriterOptions = def { writerHTMLMathMethod = MathJax "/mathjax",
                        writerHighlight = True
                      }


-- constant file paths
problemDirPath :: FilePath
problemDirPath = "problems"

problemSetPath :: FilePath
problemSetPath= "problemset.md"
