--------------------------------------------------------------------
-- Test Python code using doctests and mutation testing of doctests 
--------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
module Codex.Tester.PythonDoctest (
  pythonDocTester, pythonMuDocTester
  ) where

import           Codex.Tester

import           Data.Text(Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Data.Maybe(maybeToList)
import           System.Directory (doesFileExist)

import qualified Text.Pandoc.Builder as P


pythonDocTester :: Tester Result
pythonDocTester = tester "doctest" $ do
  Code lang src <- testCode
  guard (lang == "python")
  python    <- configured "language.python.interpreter"
  runtests  <- configured "language.python.runtests"
  limits    <- configLimits "language.python.limits"
  dir <- takeDirectory <$> testFilePath
  -- "old-style" single doctest file
  old_tests <- fmap (dir</>) <$> metadata "tests"
  new_tests <- maybe [] (map (dir</>)) <$> metadata "public-tests"
  let pub_tests = maybeToList old_tests ++ new_tests  
  priv_tests <- maybe [] (map (dir</>)) <$> metadata "private-tests"
  --
  missing <- liftIO $
             filterM (fmap not . doesFileExist) (pub_tests ++ priv_tests)
  if null missing then
    withTemp "submit.py" src $ \pyfile -> do
       chmod readable pyfile
       r <- runDoctests limits python [runtests, pyfile] pub_tests
       r'<- runDoctests limits python [runtests, pyfile] priv_tests
       return (tagWith Public r <> tagWith Private r')
    else
       return
         (miscError (P.plain (P.text ("Cannot find doctest files: " <>
                                       T.pack (show missing)))))


runDoctests :: Limits -> FilePath -> [String] -> [String] -> IO Result
runDoctests _      _      _    []     = return mempty
runDoctests limits python args tests = do
  let cmdline = args ++ tests
  classify <$> safeExec limits python Nothing cmdline ""


-- classify a doctest run
classify :: (ExitCode, Text, Text) -> Result
classify (ExitSuccess, stdout, _)      = accepted (P.plain (P.str stdout))
classify (ExitFailure _, stdout, stderr)
  | match "Time Limit" stderr          = timeLimitExceeded (P.codeBlock stderr)
  | match "Memory Limit" stderr        = memoryLimitExceeded (P.codeBlock stderr)
  | match "SyntaxError" stderr         = compileError (P.codeBlock stderr)
  | match "Exception raised" stdout    = runtimeError (P.codeBlock stdout')
  | match "Failed" stdout              = wrongAnswer (P.codeBlock stdout')
  | otherwise                          = miscError (P.codeBlock (stdout' <> stderr))
  where stdout' = filterPaths stdout
        

-- filter doctest filepaths from report;
-- we need this to avoid leaking server info to students
filterPaths :: Text -> Text
filterPaths = T.unlines . filter (not . T.isPrefixOf "File \"") . T.lines

---------------------------------------------------------------------------
-- Mutation testing: check a test suite against mutants
---------------------------------------------------------------------------

pythonMuDocTester :: Tester Result
pythonMuDocTester = tester "mudoctest" $ do
  Code lang src <- testCode
  guard (lang == "python")
  python    <- configured "language.python.interpreter"
  runtests  <- configured "language.python.runtests"
  limits    <- configLimits "language.python.limits"
  dir <- takeDirectory <$> testFilePath
  limit_tests <- metadata "limit-tests"
  targetPath <- maybe undefined (dir</>) <$> metadata "target"
  target <- liftIO $ T.readFile targetPath
  mutants <- globPatterns dir =<< metadataWithDefault "mutants" []
  withTemp "submit.tst" src $ \tstfile -> do
    chmod readable tstfile
    r <- runTarget target limits python runtests limit_tests tstfile targetPath
    case resultStatus r of
      MiscError -> return r
      _ -> do
        rs <- runMutants target limits python runtests limit_tests tstfile mutants
        return (r <> mconcat rs)

runTarget :: Text -> Limits -> FilePath -> FilePath -> Maybe Int
          -> FilePath -> FilePath
          -> IO Result
runTarget target limits python runtests limit_tests tstfile targetPath = do
  let cmdline = [runtests, "--verbose", "--all-fails"] ++
                maybe [] (\n -> ["--limit-tests", show n]) limit_tests ++
                [targetPath, tstfile]
  classifyTarget target <$> safeExec limits python Nothing cmdline ""

runMutants :: Text 
           -> Limits -> FilePath -> FilePath -> Maybe Int
           -> FilePath -> [FilePath]
           -> IO [Result]
runMutants target limits python runtests limit_tests tstfile mutants = 
  forM (zip [1..] mutants) $ \(k,mut) -> do
        let cmdline =
              [runtests, "--verbose", "--all-fails"] ++
              maybe [] (\n -> ["--limit-tests", show n]) limit_tests ++
              [mut, tstfile]
        src <- T.readFile mut
        classifyMutant k target src <$>
          safeExec limits python Nothing cmdline ""


-- | Classify a target test run
classifyTarget :: Text -> (ExitCode, Text, Text) -> Result
classifyTarget target (ExitSuccess, stdout, _)
  = accepted (P.header 3 (P.text "Target passed all tests " <> checkMark)
              <>
              P.codeBlock target
              `besides`
              P.codeBlock stdout)
    
classifyTarget target (ExitFailure _, stdout, stderr)
  | match "Maximum number of tests exceeded" stderr ||
    match "No tests defined" stderr 
  = suiteRejected stderr
  | match "Time Limit" stderr
  = timeLimitExceeded (rejected stderr)
  | match "Memory Limit" stderr
  = memoryLimitExceeded (rejected stderr)
  | match "SyntaxError" stderr
  = compileError (rejected stderr)
  | match "Exception raised" stdout
  = runtimeError (rejected stdout')
  | match "Failed" stdout
  = wrongAnswer (rejected stdout')
  | otherwise
  = miscError (rejected (stdout' <> stderr))
  where
    stdout' = filterPaths stdout
    rejected txt
      = P.header 3 (P.str "Target failed some tests " <> crossMark)
        <> P.codeBlock target `besides`
           P.codeBlock txt



-- | Classify a mutant test run
classifyMutant :: Int -> Text -> Text -> (ExitCode, Text, Text) -> Result
classifyMutant k target mut (ExitSuccess, stdout, _)      
   = wrongAnswer (P.header 3 (P.text "Mutated version " <>
                              P.str (T.pack $ show k) <>
                              P.text " not rejected " <>
                              crossMark)
                   <> textDiffs target mut
                      `besides`
                      P.codeBlock stdout'
                 )
  where stdout' = filterPaths stdout
classifyMutant k target mut (ExitFailure _, stdout, stderr)
  | match "Maximum number of tests exceeded" stderr ||
    match "No tests defined" stderr
  = suiteRejected stderr
  | otherwise
  = accepted (P.header 3 (P.text "Mutated version " <>
                          P.str (T.pack $ show k) <>
                          P.text " rejected " <>
                          checkMark)
                <> textDiffs target mut
                    `besides`
                    P.codeBlock stdout'
              )
  where stdout' = filterPaths stdout
  

suiteRejected :: Text -> Result
suiteRejected msg
  = miscError (P.header 2 (P.text "Test suite rejected")
               <>
               P.codeBlock msg)


textDiffs :: Text -> Text -> P.Blocks
textDiffs from to
  = P.divWith ("",["text-diffs"],[]) (P.plain $ formatDiffs from to)

besides :: P.Blocks -> P.Blocks -> P.Blocks
besides left right
  = P.divWith ("", ["twocolumn"], [])
     (P.divWith ("",["left"], []) left
       <>
      P.divWith ("",["right"], []) right)

-- Dingbats
checkMark, crossMark :: P.Inlines
checkMark = P.spanWith ("",["Accepted"],[]) (P.str "\x2713")
crossMark = P.spanWith ("",["WrongAnswer"],[]) (P.str "\x2718")
