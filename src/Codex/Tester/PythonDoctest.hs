--------------------------------------------------
-- Test Python code using doctest scripts
--------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
module Codex.Tester.PythonDoctest (
  pythonDocTester
  ) where

import           Codex.Tester
import           Data.Text(Text)
import qualified Data.Text as T
import           Data.Maybe(fromMaybe)

pythonDocTester :: Tester Result
pythonDocTester = tester "doctest" $ do
  Code lang src <- testCode
  guard (lang == "python")
  python    <- configured "language.python.interpreter"
  runtests  <- configured "language.python.runtests"
  limits    <- configLimits "language.python.limits"
  dir <- takeDirectory <$> testFilePath
  pub_tests <- map (dir</>) . fromMaybe [] <$> metadata "public"
  priv_tests <- map (dir</>) . fromMaybe [] <$> metadata "private"  
  withTemp "submit.py" src $ \pyfile -> do
    chmod readable pyfile
    r <- public <$> runDoctests limits python [runtests, pyfile] pub_tests
    r'<- private <$> runDoctests limits python [runtests, pyfile] priv_tests
    return (r <> r')


runDoctests :: Limits -> FilePath -> [String] -> [FilePath] -> IO Result
runDoctests _      _      _    []     = return mempty
runDoctests limits python args tests = do
  let cmdline = args ++ tests
  classify <$> safeExec limits python Nothing cmdline ""
      


classify :: (ExitCode, Text, Text) -> Result
classify (ExitSuccess, stdout, _)      = accepted (filterPaths stdout)
classify (ExitFailure _, stdout, stderr)
  | match "Time Limit" stderr          = timeLimitExceeded stderr
  | match "Memory Limit" stderr        = memoryLimitExceeded stderr
  | match "SyntaxError" stderr         = compileError stderr
  | match "Exception raised" stdout    = runtimeError (filterPaths stdout)
  | match "Failed" stdout              = wrongAnswer (filterPaths stdout)
  | otherwise                          = miscError (filterPaths stdout <> stderr)


-- filter doctest filepaths from report
filterPaths :: Text -> Text
filterPaths = T.unlines . filter (not . T.isPrefixOf "File \"") . T.lines

-- label reports as private or public
private :: Result -> Result
private r
  = r { resultReport = "Private tests: " `label` lastLine (resultReport r) }

public :: Result -> Result
public r
  = r { resultReport = "Public tests: " `label` resultReport r }

label :: Text -> Text -> Text
label msg info | T.null info = ""
               | otherwise   = msg <> info
             
lastLine :: Text -> Text
lastLine txt = if T.null txt then "" else last (T.lines txt)
