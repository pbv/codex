--------------------------------------------------------------------
-- Test SICStus Prolog code using PL-Unit
--------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
module Codex.Tester.PrologUnit (
  plUnitTester
  ) where

import           Codex.Tester
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Text.Regex.TDFA ( Regex, makeRegex, matchTest )
import           Data.Maybe(fromMaybe, maybeToList)
import qualified Text.Pandoc.Builder as P 

plUnitTester :: Tester Result
plUnitTester = tester "plunit" $ do
  Code lang src <- testCode
  guard (lang == "prolog")
  prolog <- configured "language.prolog.interpreter" 
  profile <- configured "language.prolog.firejail"
  testDir <- takeDirectory <$> testFilePath
  -- public and private tests
  pubTests <- metadata "public-tests"
  privTests <- metadata "private-tests"
  extraFiles <- fromMaybe [] <$> metadata "extra-files"
  withTempDir "codex" $ \tmpdir -> do
    let plfile = tmpdir </> "submission.pl"
    T.writeFile plfile src
    let allFiles = map (testDir</>)
                   (maybeToList pubTests ++ maybeToList privTests ++ extraFiles)
    copyFiles allFiles tmpdir
    r <- runTests prolog profile tmpdir (takeFileName <$> pubTests)
    r'<- runTests prolog profile tmpdir (takeFileName <$> privTests)
    return (tagWith Public r <> tagWith Private r')


runTests :: FilePath -> FilePath -> FilePath -> Maybe String -> IO Result
runTests prolog profile dir optTest
  = case optTest of
      Nothing -> return mempty
      Just test ->
        classify <$>
        sandboxExec profile prolog (Just dir) ["--nologo", "-l", test] ""


-- classify a PL-unit process run
classify :: ProcessRun -> Result
classify (ProcessRun (ExitFailure _) stdout stderr)
  | match "TimeLimitExceed" stderr = timeLimitExceeded (P.codeBlock msg)
  | match "out of memory" stderr = memoryLimitExceeded (P.codeBlock msg)
  | otherwise                    = miscError (P.codeBlock msg)
  where msg = filterOutput (stdout <> stderr)
classify (ProcessRun ExitSuccess stdout stderr)
  | match "! Syntax error" stderr = compileError (P.codeBlock msg)
  | match "tests failed" stderr = wrongAnswer (P.codeBlock msg)
  | otherwise                   = accepted (P.codeBlock msg)
  where msg = filterOutput (stdout <> stderr)
    

-- | filter PL-unit output to remove clutter
filterOutput :: Text -> Text
filterOutput = T.unlines . filter keep . T.lines
  where
    keep :: Text -> Bool
    keep txt = not (matchTest discard txt)

discard :: Regex
discard = makeRegex ("^%(.)*(compiling|compiled|loading|module|consulting|consulted)"  :: Text)
