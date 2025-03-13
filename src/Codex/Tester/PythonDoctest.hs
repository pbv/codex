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
import           Data.Maybe(maybeToList)
import           System.Directory (doesFileExist)

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
  new_tests <- maybe [] (map (dir</>)) <$> metadata "public"
  let pub_tests = maybeToList old_tests ++ new_tests  
  priv_tests <- maybe [] (map (dir</>)) <$> metadata "private"
  --
  missing <- liftIO $ filterM (fmap not . doesFileExist) (pub_tests ++ priv_tests) 
  if null missing then
    withTemp "submit.py" src $ \pyfile -> do
       chmod readable pyfile
       r <- runDoctests limits python [runtests, pyfile] pub_tests
       r'<- markPrivate <$> runDoctests limits python [runtests, pyfile] priv_tests
       return (r <> r')
    else
       return (miscError ("missing doctest files: " <> T.pack (show missing)))
                          



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


-- filter doctest filepaths from report;
-- we need this to avoid leaking server info to students
filterPaths :: Text -> Text
filterPaths = T.unlines . filter (not . T.isPrefixOf "File \"") . T.lines
