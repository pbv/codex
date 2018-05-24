{-# LANGUAGE OverloadedStrings #-}
{-
-- Test Python code using a doctest script
-}
module Codex.Tester.Python (
  pythonDoctester
  ) where

import           Codex.Tester
import           Data.Text(Text)
import qualified Data.Text as T


pythonDoctester :: PageInfo -> Code -> Test Result
pythonDoctester (PageInfo path meta) (Code lang src) = do
  guard (lang == "python")
  guard (tester meta == Just "doctest")
  ---
  python  <- configured "language.python.interpreter"
  pytest  <- configured "language.python.pytest"
  scripts <- configured "language.python.scripts"
  limits  <- getLimits "language.python.limits"
  let doctestPath = replaceExtension path ".tst"
  assert (fileExists doctestPath)
    ("doctest file not found: " <> show doctestPath)
  chmod readable doctestPath
  withTemp "submit.py" src $ \pyfile -> do
    chmod readable pyfile
    classify <$>
      safeExec limits python [pytest, scripts, doctestPath, pyfile] ""


classify :: (ExitCode, Text, Text) -> Result
classify (_, stdout, stderr)
  | T.null stdout && match "OK" stderr = accepted stderr
  | match "Time Limit" stderr          = timeLimitExceeded stderr
  | match "Memory Limit" stderr        = memoryLimitExceeded stderr
  | match "Exception Raised" stdout    = runtimeError stdout
  | match "SyntaxError" stderr         = compileError stderr
  | match "Failed" stdout              = wrongAnswer stdout
  | otherwise                          = miscError (stdout <> stderr)



{-
-- | guess the doctest path from page metadata or filename
guessDoctest :: FilePath -> Meta -> FilePath
guessDoctest filepath meta
  = maybe
       (replaceExtension filepath ".tst")
       (takeDirectory filepath </>)
       (lookupFromMeta "doctest" meta)
-}
  
