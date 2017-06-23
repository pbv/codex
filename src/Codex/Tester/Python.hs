{-# LANGUAGE OverloadedStrings #-}
module Codex.Tester.Python (
  pythonTester
  ) where

import           System.FilePath
import           System.Exit
import           System.Directory (doesFileExist)
import           Codex.Tester
import           Codex.Page
import           Data.Text(Text)
import qualified Data.Text as T


pythonTester :: Tester Result
pythonTester = withLanguage "python" $ \code -> do
  python <- configured "language.python.interpreter"
  pytest <- configured "language.python.pytest"
  scripts<- configured "language.python.scripts"
  limits <- testerLimits "language.python.limits"
  safeexec <- testerSafeExec
  page <- testerPage
  path <- testerPath
  let tstfile = guessDoctest path page
  liftIO $ do
    c <- doesFileExist tstfile
    if c then do
      ensureFileReadable tstfile 
      withTextTemp "tmp.py" code $ \pyfile ->
        pythonResult <$>
        safeExecWith safeexec limits python [pytest, scripts, tstfile, pyfile] ""
      else return (miscError $ T.pack $ "missing doctest file: " ++ tstfile)



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

  
