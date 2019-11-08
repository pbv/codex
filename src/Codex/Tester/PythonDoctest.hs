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
  Code lang src <- testCode
  guard (lang == "python")
  ---
  python    <- configured "language.python.interpreter"
  runtests  <- configured "language.python.runtests"
  scripts   <- configured "language.python.scripts"
  limits    <- configLimits "language.python.limits"
  linterOpt <- maybeConfigured "language.python.linter" >>= traverse parseArgs
  linterFlag <- fromMaybe False <$> metadata "linter"
  path  <- testFilePath
  tstpath <- fromMaybe (replaceExtension path ".tst") <$>
              metadataPath "tests"
  assert (fileExists tstpath)
    ("tests file not found: " <> show tstpath)
  chmod readable tstpath
  withTemp "submit.py" src $ \pyfile -> (do
    chmod readable pyfile
    when linterFlag $
      case linterOpt of
        Just (cmd:args) -> runCompiler Nothing cmd (args ++ [pyfile])
        _ -> fail "missing python linter command in config file"
    let args = [runtests, scripts, tstpath, pyfile]
    classify <$> safeExec limits python Nothing args "") `catch` return


classify :: (ExitCode, Text, Text) -> Result
classify (ExitSuccess, stdout, _)      = accepted stdout  
classify (ExitFailure _, stdout, stderr)
  | match "Time Limit" stderr          = timeLimitExceeded stderr
  | match "Memory Limit" stderr        = memoryLimitExceeded stderr
  | match "SyntaxError" stderr         = compileError stderr
  | match "Exception raised" stdout    = runtimeError stdout
  | match "failed" stdout              = wrongAnswer stdout
  | otherwise                          = miscError (stdout <> stderr)



