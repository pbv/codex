--------------------------------------------------
-- Test Python code using a doctest script
--------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
module Codex.Tester.PythonDoctest (
  pythonDocTester
  ) where

import           Codex.Tester
import           Data.Text(Text)
import qualified Data.Text as T
import           Data.Maybe(fromMaybe)
import           Control.Exception (catch)


pythonDocTester :: Tester Result
pythonDocTester = tester "doctest" $ do
  Code lang src <- askSubmitted
  guard (lang == "python")
  ---
  python    <- configured "language.python.interpreter"
  pytest    <- configured "language.python.pytest"
  scripts   <- configured "language.python.scripts"
  optLinter <- maybeConfigured "language.python.linter"
  limits  <- askLimits "language.python.limits"
  path    <- askPath
  testsPath <- fromMaybe (replaceExtension path ".tst")
               <$>
               metadataPath "tests"
  assert (fileExists testsPath)
    ("tests file not found: " <> show testsPath)
  chmod readable testsPath
  withTemp "submit.py" src $ \pyfile -> (do
    chmod readable pyfile
    case optLinter of
      Just linter -> runCompiler linter [pyfile]
      Nothing -> return ()
    classify <$>
      safeExec limits python [pytest, scripts, testsPath, pyfile] "") `catch` return


classify :: (ExitCode, Text, Text) -> Result
classify (_, stdout, stderr)
  | T.null stdout && match "OK" stderr = accepted stderr
  | match "Time Limit" stderr          = timeLimitExceeded stderr
  | match "Memory Limit" stderr        = memoryLimitExceeded stderr
  | match "Exception raised" stdout    = runtimeError stdout
  | match "SyntaxError" stderr         = compileError stderr
  | match "Failed" stdout              = wrongAnswer stdout
  | otherwise                          = miscError (stdout <> stderr)

