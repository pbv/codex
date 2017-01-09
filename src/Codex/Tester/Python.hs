{-# LANGUAGE OverloadedStrings #-}
module Codex.Tester.Python (
  pythonTester
  ) where

import           System.FilePath
import           System.Exit
import           System.Directory (doesFileExist)
import           Codex.Tester
import           Codex.Page
import           Codex.Markdown
import           Codex.Config
import           Codex.SafeExec
import           Data.Text(Text)
import qualified Data.Text as T
import qualified Data.Configurator as Configurator


pythonTester :: Tester Result
pythonTester = language "python" $ \code -> do
    conf <- getConfig
    page <- getPage
    liftIO $ do
      python <- Configurator.require conf "language.python.interpreter"
      sf <- getSafeExecConf (Configurator.subconfig "safeexec" conf)
      let tstfile = publicPath </> getDoctest page
      c <- doesFileExist tstfile
      if c then withTextTemp "tmp.py" code $
                \pyfile ->
                    pythonResult <$>
                    safeExecWith sf python ["python/pytest.py", tstfile, pyfile] ""
           else return (miscError $ T.pack $ "missing doctest file: " ++ tstfile)



pythonResult :: (ExitCode, Text, Text) -> Result
pythonResult (_, stdout, stderr)
  | T.null stdout && match "OK" stderr = accepted stderr
  | match "Time Limit" stderr          = timeLimitExceeded stderr
  | match "Memory Limit" stderr        = memoryLimitExceeded stderr
  | match "Exception Raised" stdout    = runtimeError stdout
  | match "SyntaxError" stderr         = compileError stderr
  | match "Failed" stdout              = wrongAnswer stdout
  | otherwise                   = miscError (stdout `T.append` stderr)



-- | guess the relative doctest path from metada or the filename
getDoctest :: Page -> FilePath
getDoctest p
  = let path = pagePath p
        meta = pageMeta p
    in maybe
       (replaceExtension path ".tst")
       (takeDirectory path </>)
       (lookupFromMeta "doctest" meta)

  
