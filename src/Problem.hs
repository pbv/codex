{-# LANGUAGE OverloadedStrings #-}
{-
  Data types and methods for problems and submissions
-}

module Problem where

import           Prelude hiding(catch)
import           Data.List(sort)
import           System.Locale(defaultTimeLocale)
import           Data.Time.Clock
import           Data.Time.Format
import           Data.Time.LocalTime
import           Control.Monad
import           Control.Applicative((<$>))
import           System.FilePath
import           System.Directory
-- import           Data.ByteString.UTF8(ByteString)
import qualified Data.ByteString.UTF8 as B
import           Text.XmlHtml(Node)
import qualified Text.XmlHtml          as X
import           Data.Text(Text)
import qualified Data.Text             as T

import           Types 
import           XHTML
import           Text.Parsec
--import           Text.Parsec.Combinator


-- datatype for problems 
-- parameterized by type of times for parsing flexibility 
data Problem t = Problem {
  probID     :: PID,            -- unique id (from filepath)
  probTitle  :: Text,           -- title
  probDescr  :: [Node],         -- description (HTML nodes)
  probSubmit :: Text,           -- optional default submission text
  probStart  :: Maybe t,        -- optional start time
  probEnd    :: Maybe t,        -- optional end time
  probExam :: Bool              -- visible in the submission interval
  } deriving Show

-- Functor instance for applying functions to the time fields
instance Functor Problem where
  fmap f p = p { probStart = fmap f (probStart p),
                 probEnd   = fmap f (probEnd p) 
               }

instance Eq (Problem t) where
  p == p' = probID p == probID p'

{-
instance Ord (Problem t) where
  compare p p' = compare (probID p) (probID p')  
  -}

-- ordering instance: lexicographically compare by times then by id
instance Ord t => Ord (Problem t) where  
  compare p p' = compare (f p) (f p')
    where f x = (probStart x, probEnd x, probID x)

  

-- XML reader for problems
  {-
problemReader :: PID -> XMLReader (Problem LocalTime)
problemReader pid = do
  whitespace   
  title <- tagged "problem" allText
  whitespace   
  descr <- tagged "description" (many anyToken)
  whitespace   
  subm <- option "" (tagged "submitText" allText)
  whitespace
  start <- optionMaybe (tagged "startTime" localTime)
  whitespace   
  end <- optionMaybe (tagged "endTime" localTime)
  return (Problem pid title descr subm start end)
-}

newProblem :: PID -> Problem LocalTime
newProblem pid = Problem { probID    = pid
                         , probTitle = T.pack ("Problem " ++ show pid)
                         , probDescr = []
                         , probSubmit= ""
                         , probStart = Nothing
                         , probEnd   = Nothing
                         , probExam  = False 
                         }


-- problem parser; top-level wrapper function
problemReader :: PID -> XMLReader (Problem LocalTime)
problemReader pid = 
  do whitespace
     p <- problemReader' (newProblem pid)
     whitespace
     empty
     return p
        
-- worker function
problemReader' :: Problem LocalTime -> XMLReader (Problem LocalTime)
problemReader' p 
  = do title <- tagged "problem" allText
       problemReader' p {probTitle=title}
  <|> do descr <- tagged "description" nodes
         problemReader' p {probDescr=descr}
  <|> do text <- tagged "submitText" allText
         problemReader' p {probSubmit=text}
  <|> do t <- tagged "startTime" localTime
         problemReader' p {probStart=Just t}
  <|> do t <- tagged "endTime" localTime
         problemReader' p {probEnd=Just t}
  <|> do tagged "exam" empty
         problemReader' p {probExam=True}
  <|> return p
  


-- read a local time
localTime :: XMLReader LocalTime
localTime = do txt <- allText 
               let str = T.unpack txt
               let mb = msum [parseTime defaultTimeLocale fmt str | fmt<-dateFormats]
               case mb of
                 Nothing -> fail "localTime: no parse"
                 Just t -> return t

dateFormats :: [String]
dateFormats = ["%H:%M %d/%m/%Y", "%d/%m/%Y", "%c"]




-- get all problem IDs
-- filter HTML files to get problem IDs
getProblemIDs :: IO [PID]
getProblemIDs = do
  files <- filter ((==".html").snd.splitExtension) <$> getDirectoryContents "problems"
  return $ map (PID . B.fromString . dropExtension) files


-- get all available problems 
getProblems ::  IO [Problem UTCTime]
getProblems  = getProblemIDs >>= fmap sort . mapM getProblem 

getProblem :: PID -> IO (Problem UTCTime)
getProblem pid = readProblemFile pid fp
  where fp = "problems" </> show pid <.> "html"
    


-- read an html problem file
readProblemFile :: PID -> FilePath -> IO (Problem UTCTime)
readProblemFile pid fp
  = do doc <- readHTMLFile fp
       case parse (problemReader pid) fp (X.docContent doc) of
         Left msg -> ioError $ userError $ show msg
         -- convert times to UTC
         Right prob -> do z <- getCurrentTimeZone
                          return (fmap (localTimeToUTC z) prob)
  
    
-- check if a time is within the problem's acceptance interval 
acceptable :: UTCTime -> Problem UTCTime -> Bool
acceptable t p = inside t (probStart p) (probEnd p)

-- check if a problem is visible at a given time 
-- * exam problems are visible only in the acceptance interval
-- * other problems are always visible
visible :: UTCTime -> Problem UTCTime -> Bool
visible t p = not (probExam p) || acceptable t p 


inside :: UTCTime -> Maybe UTCTime -> Maybe UTCTime -> Bool
inside t (Just t0) (Just t1) = t0<t && t<t1
inside t (Just t0) _         = t0<t
inside t _         (Just t1) = t<t1
inside _ _        _          = True


