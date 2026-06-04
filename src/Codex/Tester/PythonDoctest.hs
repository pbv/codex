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
import           Data.Maybe(fromMaybe, maybeToList)
-- import           System.Directory (copyFile)

import qualified Text.Pandoc.Builder as P

pythonDocTester :: Tester Result
pythonDocTester = tester "doctest" $ do
  Code lang src <- testCode
  guard (lang == "python")
  python    <- configured "language.python.interpreter"
  runtests  <- configured "language.python.runtests"
  profile   <- configured "language.python.firejail"
  testDir <- takeDirectory <$> testFilePath
  -- old-style single doctest file
  oldTests <- metadata "tests"
  -- new-style lists of public and private tests
  newTests <- fromMaybe []  <$> metadata "public-tests"
  let pubTests = maybeToList oldTests ++ newTests 
  privTests <- fromMaybe [] <$> metadata "private-tests"
  extraFiles <- fromMaybe [] <$> metadata "extra-files"
  withTempDir "codex" $ \tmpdir -> do
    let pyfile = tmpdir </> "submit.py"
    T.writeFile pyfile src
    copyFiles (map (testDir</>) (pubTests ++ privTests ++ extraFiles))
              tmpdir
    r <- runDoctests profile python tmpdir [runtests, pyfile]
                (map (tmpdir</>) pubTests)
    r'<- runDoctests profile python tmpdir [runtests, pyfile]
                (map (tmpdir</>) privTests)
    return (tagWith Public r <> tagWith Private r')


runDoctests ::
  FilePath -> FilePath -> FilePath -> [String] -> [String] -> IO Result
runDoctests _   _       _       _     []     = return mempty
runDoctests profile python dir args tests = 
  classify <$> sandboxExec profile python (Just dir) (args ++ tests) ""


-- classify a doctest process run
classify :: ProcessRun -> Result
classify (ProcessRun ExitSuccess stdout _) = accepted (P.plain (P.str stdout))
classify (ProcessRun (ExitFailure _) stdout stderr)
  | match "TimeLimitExceeded" stderr = timeLimitExceeded (P.codeBlock msg)
  | match "SyntaxError" msg          = compileError (P.codeBlock msg)
  | match "MemoryError" msg          = memoryLimitExceeded (P.codeBlock msg)
  | match "Exception raised" msg     = runtimeError (P.codeBlock msg)
  | match "Failed" msg               = wrongAnswer (P.codeBlock msg)
  | otherwise                        = miscError (P.codeBlock msg)
  where msg = filterPaths stdout <> stderr
        

-- filter doctest filepaths from report;
-- avoid leaking server info to students
filterPaths :: Text -> Text
filterPaths = T.unlines . filter (not . T.isPrefixOf "File \"") . T.lines

---------------------------------------------------------------------------
-- Mutation testing: check a test suite against mutants
---------------------------------------------------------------------------

pythonMuDocTester :: Tester Result
pythonMuDocTester = fail "temporarily disabled"
{-
pythonMuDocTester = tester "mudoctest" $ do
  Code lang src <- testCode
  guard (lang == "python")
  python    <- configured "language.python.interpreter"
  runtests  <- configured "language.python.runtests"
  -- limits    <- configLimits "language.python.limits"
  profile <- configured "language.python.firejail"
  dir <- takeDirectory <$> testFilePath
  limit_tests <- metadata "limit-tests"
  targetPath <- maybe undefined (dir</>) <$> metadata "target"
  target <- liftIO $ T.readFile targetPath
  mutants <- globPatterns dir =<< metadataWithDefault "mutants" []
  withTemp "submit.tst" src $ \tstfile -> do
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
-}
