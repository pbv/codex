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
  linter    <- fromMaybe False <$> metadata "linter"
  linterCmd <- fmap words <$> maybeConfigured "language.python.linter"
  extraArgs <- (words . fromMaybe "") <$> metadata "linter-options"
  limits  <- askLimits "language.python.limits"
  path    <- askPath
  testsPath <- fromMaybe (replaceExtension path ".tst") <$>
               metadataPath "tests"
  assert (fileExists testsPath)
    ("tests file not found: " <> show testsPath)
  chmod readable testsPath
  withTemp "submit.py" src $ \pyfile -> (do
    chmod readable pyfile
    when linter $
      case linterCmd of
        Just (cmd:args') ->
          runCompiler cmd (args' ++ extraArgs ++ [pyfile])
        _ -> fail "linter command not found in config file"
    classify <$>
      safeExec limits python [pytest, scripts, testsPath, pyfile] "") `catch` return


classify :: (ExitCode, Text, Text) -> Result
classify (_, stdout, stderr)
  | match "0 failed" stdout && match "OK" stderr = accepted (stdout <> stderr)
  | match "Time Limit" stderr          = timeLimitExceeded stderr
  | match "Memory Limit" stderr        = memoryLimitExceeded stderr
  | match "Exception raised" stdout    = runtimeError stdout
  | match "SyntaxError" stderr         = compileError stderr
  | match "Failed" stdout              = wrongAnswer stdout
  | otherwise                          = miscError (stdout <> stderr)

