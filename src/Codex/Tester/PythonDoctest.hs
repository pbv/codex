--------------------------------------------------
-- Test Python code using a doctest script
--------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
module Codex.Tester.PythonDoctest (
  pythonDocTester
  ) where

import           Codex.Tester
import           Data.Text(Text)
import           Data.Maybe(fromMaybe)
import           Control.Exception (catch)


pythonDocTester :: Tester Result
pythonDocTester = tester "doctest" $ do
  Code lang src <- askSubmitted
  guard (lang == "python")
  ---
  python    <- configured "language.python.interpreter"
  runtests  <- configured "language.python.runtests"
  scripts   <- configured "language.python.scripts"
  limits    <- askLimits "language.python.limits"
  linterOpt <- maybeConfigured "language.python.linter" >>=
               traverse parseArgs
  linterFlag <- fromMaybe False <$> metadata "linter"
  path    <- askPath
  testsPath <- fromMaybe (replaceExtension path ".tst") <$>
               metadataPath "tests"
  assert (fileExists testsPath)
    ("tests file not found: " <> show testsPath)
  chmod readable testsPath
  withTemp "submit.py" src $ \pyfile -> (do
    chmod readable pyfile
    when linterFlag $
      case linterOpt of
        Just (cmd:args) -> runCompiler cmd (args ++ [pyfile])
        _ -> fail "missing python linter command in config file"
    classify <$>
      safeExec limits python Nothing [runtests, scripts, testsPath, pyfile] "") `catch` return


classify :: (ExitCode, Text, Text) -> Result
classify (_, stdout, stderr)
  | match "Time Limit" stderr          = timeLimitExceeded stderr
  | match "Memory Limit" stderr        = memoryLimitExceeded stderr
  | match "Exception raised" stdout    = runtimeError stdout
  | match "SyntaxError" stderr         = compileError stderr
  | match "Command exited with non-zero status" stderr
                                       = wrongAnswer stdout
  | match " 0 failed" stdout           = accepted stdout
  | otherwise                          = miscError (stdout <> stderr)

