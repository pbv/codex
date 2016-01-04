{-# LANGUAGE OverloadedStrings, RecordWildCards, DeriveFunctor #-}
{-# LANGUAGE ExistentialQuantification, FlexibleContexts #-}
{-
  Data types and methods for problems 
-}

module Problem ( 
  Problem(..),
  Worksheet(..),
  readWorksheet,      -- read a worksheet
  ) where

import           Control.Monad
import           Control.Monad.State
import           Control.Monad.Trans
import           Control.Applicative ((<$>))

import qualified Data.ByteString.UTF8 as B

import           Data.String
import           Data.Text(Text)
import qualified Data.Text             as T


import           Data.Time.LocalTime
import           Data.Time.Format
import           Data.Time.Clock
import           System.Locale (defaultTimeLocale)

import           Data.Aeson (encode)
import           Snap.Snaplet.SqliteSimple

import           Markdown
import           Text.Pandoc hiding (Code)
import           Text.Pandoc.Builder hiding (Code)
-- import           Data.Monoid
-- import           Text.XmlHtml 
-- import           Text.Blaze.Renderer.XmlHtml
import           Application
import           Types
import           Language
import           Tester

-- import           ParseMeta
-- import           LogIO
-- import           System.Directory(doesFileExist)


-- individual problems
data Problem = Problem {
  probID       :: ProblemID,       -- unique identifier
  probHeader   :: Block,           -- header and description 
  probDescr    :: Blocks,
  probSubmit   :: Maybe Code,       -- optional default code
  probAttrs    :: [(Text, Text)],   -- attributes (key-value pairs)
  probLimit    :: Maybe UTCTime,     -- optional deadline
  probTester   :: Tester AppHandler  -- tester in the application monad
  } 

-- worksheets
data Worksheet a = Worksheet { worksheetMeta :: Meta
                             , worksheetItems :: [Either Blocks a]
                             }
                 deriving (Show, Functor)

-- split a list of blocks into worksheet items
splitItems :: [Block] -> [Either Blocks [Block]]
splitItems [] = []
splitItems (block : blocks)
  | problemStart block = Right (block:blocks') : splitItems blocks''
   where (blocks', blocks'') = break problemEnd blocks
splitItems (block : blocks)
  = Left (fromList (block:blocks')) : splitItems blocks''
  where (blocks',blocks'') = break problemStart blocks 

-- checkers for problem start & end
problemStart :: Block -> Bool
problemStart (Header _ attr _) = "problem" `elem` classes attr
problemStart _                 = False

problemEnd :: Block -> Bool
problemEnd HorizontalRule    = True
problemEnd block             = problemStart block


-- parse a single problem
parseProblem :: TimeZone -> [Block] -> Problem
parseProblem tz (header:blocks)
  = let (ident, classes, attrs) = headerAttr header
        lang = lookup "language" attrs
        blocks' = removeCode "submit" blocks
        (descr, tester) = case lang of
          Just "python" -> parsePythonProblem blocks'
          Just "haskell" -> parseHaskellProblem blocks'
          _         ->  (blocks',  errorTester "undefined language")
    in Problem { probID = fromString ident
               , probHeader= header
               , probDescr = fromList descr
               , probSubmit= parseCode "submit" blocks
               , probTester = tester
               , probAttrs = [(T.pack k,T.pack v) | (k,v)<-attrs]
               , probLimit = lookup "close" attrs >>= parseUTCTime tz
               }

errorTester :: Monad m => Text -> Tester m 
errorTester msg = const $ return (MiscError, msg)

parseHaskellProblem :: [Block] -> ([Block], Tester AppHandler)
parseHaskellProblem blocks
  = (descr, maybe (errorTester "missing quickcheck block") haskellTester props)
  where descr = removeCode "quickcheck" blocks
        props = parseTests "quickcheck" blocks

parsePythonProblem blocks
  = (descr, maybe (errorTester "missing doctest block") pythonTester tests)
  where descr = removeCode "doctest" blocks
        tests = parseTests "doctest" blocks


removeCode :: String -> [Block] -> [Block]
removeCode tag = filter (not . tagged)
  where tagged (CodeBlock attr _) = tag `elem` classes attr 
        tagged _ = False


parseCode :: String -> [Block] -> Maybe Code
parseCode tag bs = Code <$> parseCodeBlock tag bs

parseTests :: String -> [Block] -> Maybe Tests
parseTests tag bs = Tests <$> parseCodeBlock tag bs

parseCodeBlock :: String -> [Block] -> Maybe Text
parseCodeBlock tag bs
  = case cs of [] -> Nothing
               _  -> Just (T.concat cs)
  where cs = [T.pack txt | CodeBlock attr txt <- bs, tag `elem` classes attr]


-- parse time strings
parseLocalTime :: String -> Maybe LocalTime
parseLocalTime txt 
  = msum [parseTime defaultTimeLocale fmt txt | fmt<-timeFormats] 
  where timeFormats = ["%H:%M %d/%m/%Y", "%d/%m/%Y", "%c"]

parseUTCTime :: TimeZone -> String -> Maybe UTCTime
parseUTCTime tz txt = localTimeToUTC tz <$> parseLocalTime txt 


readWorksheet :: FilePath -> IO (Worksheet Problem)
readWorksheet filepath = do
  tz <- getCurrentTimeZone
  txt <- readFile filepath
  let Pandoc meta blocks = readMarkdown myReaderOptions txt
  return (Worksheet meta (map (parseProblem tz <$>) (splitItems blocks)))



{-
-- | collect all tags from problems and problem sets
instance Tagged Problem where
    taglist = probTags

instance Tagged a => Tagged [a] where
    taglist l = Set.toList $ Set.fromList $ concatMap taglist l

instance Tagged ProblemSet where
    taglist ProblemSet{..} = dynamic ++ taglist probsetProbs
      where  dynamic = ["*accepted*", "*not accepted*", 
                        "*submitted*", "*not submitted*"]
-}


{-
lookupFromMeta :: ParseMeta a => String -> Meta -> LogIO (Maybe a)
lookupFromMeta tag meta = case lookupMeta tag meta of
  Nothing -> return Nothing
  Just v -> case parseMeta v of
    Left err -> logString ("metadata " ++ show tag ++ ": " ++ err) >>
                return Nothing
    Right r -> return (Just r)
    
                   
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



  


{-
-- | update Db table of problems
updateProblem :: Problem -> AppHandler ()
updateProblem Problem{..} =
  execute "INSERT OR UPDATE problems(problem_id, attrs, time_limit) VALUES (?, ?, ?)" (probID, encode probAttrs, probLimit)
-}
