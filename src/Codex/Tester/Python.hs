{-# LANGUAGE OverloadedStrings #-}
module Codex.Tester.Python (
  pythonDoctester
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


pythonDoctester :: FilePath -> Page -> Code -> Test Result
pythonDoctester path page (Code language src) = do
  guard (language == "python")
  python  <- configured "language.python.interpreter"
  pytest  <- configured "language.python.pytest"
  scripts <- configured "language.python.scripts"
  limits <- getLimits "language.python.limits"
  let doctestPath = guessDoctest path page
  liftIO $ do
    c <- doesFileExist doctestPath
    withTextTemp "tmp.py" src $ \pyfile -> do
      ensureFileReadable doctestPath
      ensureFileReadable pyfile
      pythonResult <$>
        safeExecIO limits python [pytest, scripts, doctestPath, pyfile] ""



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

  
