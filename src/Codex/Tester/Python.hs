{-# LANGUAGE OverloadedStrings #-}
module Codex.Tester.Python (
  pythonTester
  ) where

import           System.FilePath
import           System.Exit
import           System.Directory (doesFileExist)
import           Control.Exception (throwIO)
import           Codex.Types
import           Codex.Tester
import           Codex.Page
import           Data.Text(Text)
import qualified Data.Text as T


pythonTester :: Code -> Test Result
pythonTester (Code language code) = do
  guard (language == "python")
  python <- testConfig "language.python.interpreter"
  pytest <- testConfig "language.python.pytest"
  scripts<- testConfig "language.python.scripts"
  page <- testPage
  path <- testPath
  safeExec <- testSafeExec ["language.python.limits", "limits"]
  let tstfile = guessDoctest path page
  liftIO $ do
    c <- doesFileExist tstfile
    unless c $
      throwIO (miscError $ T.pack $ "missing doctest file: " ++ tstfile)
    withTextTemp "sub.py" code $ \pyfile -> do
      ensureFileReadable tstfile 
      ensureFileReadable pyfile
      pythonResult <$> safeExec python [pytest, scripts, tstfile, pyfile] ""



pythonResult :: (ExitCode, Text, Text) -> Result
pythonResult (_, stdout, stderr)
  | T.null stdout && match "OK" stderr = accepted stderr
  | match "Time Limit" stderr          = timeLimitExceeded stderr
  | match "Memory Limit" stderr        = memoryLimitExceeded stderr
  | match "Exception Raised" stdout    = runtimeError stdout
  | match "SyntaxError" stderr         = compileError stderr
  | match "Failed" stdout              = wrongAnswer stdout
  | otherwise                          = miscError (stdout `T.append` stderr)



-- | guess the doctest path from page metadata or filename
guessDoctest :: FilePath -> Page -> FilePath
guessDoctest filepath page
  = maybe
       (replaceExtension filepath ".tst")
       (takeDirectory filepath </>)
       (lookupFromMeta "doctest" $ pageMeta page)

  
