{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
{-
  Data types and methods for problems 
-}

module Problem ( 
  Problem(..),
  listProblems,  -- * list all problem ids
  getProblems,   -- * get all problems
  getProblem,    -- * get a single problem
  isEarly,       -- * check problem's acceptance dates
  isLate,
  isAvailable,     -- * can be submited
  isAcceptable,
  isVisible,       -- * is shown in listing
  isTagged
  ) where

-- import           Prelude hiding(catch)
import           Data.List(sort)
import           System.Locale(defaultTimeLocale)
import           Data.Time.Clock
import           Data.Time.Format
import           Data.Time.LocalTime
import           Control.Monad
import           Control.Applicative((<$>))
import           System.FilePath
import           System.Directory
import           Data.ByteString.UTF8(ByteString)
import qualified Data.ByteString.UTF8 as B
import           Text.XmlHtml(Node)
import qualified Text.XmlHtml          as X
import           Data.Text(Text)
import qualified Data.Text             as T

import           Types 
import           XHTML
import           Text.Parsec



-- datatype for problems 
-- parameterized by type of times for parsing flexibility 
data Problem t = Problem {
  probID     :: PID,            -- unique id name (filepath)
  probTitle  :: Text,           -- short title
  probDescr  :: [Node],         -- longer description (HTML nodes)
  probSubmit :: Text,           -- optional default submission text
  probStart  :: Maybe t,        -- optional start time
  probEnd    :: Maybe t,        -- optional end time
  probTags   :: [Text],    -- list of tags attached to this problem
  probExam   :: Bool            -- visible only during the above interval
  } deriving Show




-- Functor instance for applying functions to the time fields
instance Functor Problem where
  fmap f p = p { probStart = fmap f (probStart p),
                 probEnd   = fmap f (probEnd p) 
               }

instance Eq (Problem t) where
  p == p' = probID p == probID p'


-- ordering instance: lexicographically compare by times then by id
instance Ord t => Ord (Problem t) where  
  compare p p' = compare (tupl p) (tupl p')  
    where tupl Problem{..} = (probEnd, probStart, probID) 

  
-- an empty problem
emptyProblem :: PID -> Problem LocalTime
emptyProblem pid = Problem { probID    = pid
                           , probTitle = T.pack ("Problem " ++ show pid)
                           , probDescr = []
                           , probSubmit= ""
                           , probStart = Nothing
                           , probEnd   = Nothing
                           , probTags  = []
                           , probExam  = False 
                           }


-- problem parser; top-level wrapper function
problemReader :: PID -> XMLReader (Problem LocalTime)
problemReader pid 
  = do blankNodes; p<-problemElems (emptyProblem pid); endDoc
       return p

-- | parse problem elements (worker function)
problemElems :: Problem LocalTime -> XMLReader (Problem LocalTime)
problemElems p  
  =  do title <- element "problem" 
        continue p{probTitle=X.nodeText title}
     <|> do descr <- element "description" 
            continue p{probDescr=X.childNodes descr}
     <|> do text <- element "submitText" 
            continue p{probSubmit=X.nodeText text}
     <|> do t <- localTime "startTime" 
            continue p{probStart=Just t}
     <|> do t <- localTime "endTime"
            continue p{probEnd=Just t}
     <|> do tags <- (T.words . X.nodeText) <$> element "tags"
            continue p { probTags = tags }
     <|> do element "exam" 
            continue p{probExam=True}
     <|> return p
  where continue p' = do blankNodes; problemElems p'

  
-- parse an element wrapping a local time string 
localTime :: Text -> XMLReader LocalTime
localTime tag = do
  n <- element tag
  let txt = T.unpack $ X.nodeText n
  let opt = msum [parseTime defaultTimeLocale fmt txt | fmt<-dateFormats]
  case opt of
    Nothing -> fail ("invalid time format for " ++ T.unpack tag)
    Just t -> return t

dateFormats :: [String]
dateFormats = ["%H:%M %d/%m/%Y", "%d/%m/%Y", "%c"]

{-
-- read a local time
localTime :: XMLReader LocalTime
localTime = do txt <- allText 
               let str = T.unpack txt
               let mb = msum [parseTime defaultTimeLocale fmt str | fmt<-dateFormats]
               case mb of
                 Nothing -> fail "localTime: no parse"
                 Just t -> return t
-}




-- get a list of all problem IDs
-- filter HTML files to get problem IDs
listProblems :: IO [PID]
listProblems = do
  files <- getDirectoryContents "problems"
  return $ map toPID $ filter isXml files
  where
    isXml = (==".xml").takeExtension
    toPID = PID . B.fromString . dropExtension

-- get all available problems 
getProblems ::  IO [Problem UTCTime]
getProblems  = listProblems >>= fmap sort . mapM getProblem 

getProblem :: PID -> IO (Problem UTCTime)
getProblem pid = readProblemFile pid fp
  where fp = "problems" </> show pid <.> "xml"
    


-- read an html problem file
readProblemFile :: PID -> FilePath -> IO (Problem UTCTime)
readProblemFile pid fp
  = do doc <- readHTMLFile fp
       case parse (problemReader pid) fp (X.docContent doc) of
         Left msg -> ioError $ userError $ show msg
         -- convert times to UTC
         Right prob -> do z <- getCurrentTimeZone
                          return (fmap (localTimeToUTC z) prob)
  
    
-- relations between problems and times
isEarly, isLate :: UTCTime -> Problem UTCTime -> Bool  
isEarly t Problem{..} = ((t<) <$> probStart) == Just True
isLate t Problem{..}  = ((t>) <$> probEnd) == Just True

-- a problem can be submited if it is not early
isAvailable :: UTCTime -> Problem UTCTime -> Bool
isAvailable t p  = not (isEarly t p)

-- check if a problem is visible at a given time 
-- * exam problems are visible only in the acceptance interval
-- * other problems are always visible
isVisible :: UTCTime -> Problem UTCTime -> Bool
isVisible t p@Problem{..} =  not (probExam && (isLate t p || isEarly t p))
      

isAcceptable :: UTCTime -> Problem UTCTime -> Bool
isAcceptable t Problem{..} 
  = ((t>) <$> probStart) /= Just False &&
    ((t<) <$> probEnd) /= Just False


-- check if a problem has every tag in a list 
isTagged :: [Text] -> Problem t -> Bool
isTagged tags Problem{..} = all (`elem`probTags) tags
