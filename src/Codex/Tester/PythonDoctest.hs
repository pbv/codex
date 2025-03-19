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
  new_tests <- maybe [] (map (dir</>)) <$> metadata "public"
  let pub_tests = maybeToList old_tests ++ new_tests  
  priv_tests <- maybe [] (map (dir</>)) <$> metadata "private"
  --
  missing <- liftIO $ filterM (fmap not . doesFileExist) (pub_tests ++ priv_tests) 
  if null missing then
    withTemp "submit.py" src $ \pyfile -> do
       chmod readable pyfile
       r <- runDoctests Public limits python [runtests, pyfile] pub_tests
       r'<- runDoctests Private limits python [runtests, pyfile] priv_tests
       return (r <> r')
    else
       return (miscError (P.plain (P.str ("Cannot find doctest files: " <> T.pack (show missing)))))


runDoctests :: Visibility -> Limits -> FilePath -> [String] -> [FilePath] -> IO Result
runDoctests _ _      _      _    []     = return mempty
runDoctests vis limits python args tests = do
  let cmdline = args ++ tests
  classify vis <$> safeExec limits python Nothing cmdline ""



classify :: Visibility -> (ExitCode, Text, Text) -> Result
classify _ (ExitSuccess, stdout, _)      = accepted (P.plain (P.str stdout))

classify vis (ExitFailure _, stdout, stderr)
  | match "Time Limit" stderr          = timeLimitExceeded (P.codeBlock stderr)
  | match "Memory Limit" stderr        = memoryLimitExceeded (P.codeBlock stderr)
  | match "SyntaxError" stderr         = compileError (P.codeBlock stderr)
  | match "Exception raised" stdout    = runtimeError (formatReport vis stdout')
  | match "Failed" stdout              = wrongAnswer (formatReport vis stdout')
  | otherwise                          = miscError (tagWith vis $ P.codeBlock (stdout' <> stderr))
  where stdout' = filterPaths stdout
        
formatReport :: Visibility -> Text -> P.Blocks
formatReport vis txt 
  = case T.lines txt of 
      [] -> mempty
      _ -> tagWith vis (P.codeBlock txt)

-- filter doctest filepaths from report;
-- we need this to avoid leaking server info to students
filterPaths :: Text -> Text
filterPaths = T.unlines . filter (not . T.isPrefixOf "File \"") . T.lines
