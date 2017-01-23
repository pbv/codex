{-# LANGUAGE OverloadedStrings #-}
module Codex.Tester.Python (
  pythonTester
  ) where

import           System.FilePath
import           System.Exit
import           System.Directory (doesFileExist)
import           Codex.Tester
import           Codex.Page
import           Codex.SafeExec
import           Data.Text(Text)
import qualified Data.Text as T
import qualified Data.Configurator as Conf


pythonTester :: Tester Result
pythonTester = language "python" $ \code -> do
    conf <- getConfig
    page <- getPage
    path <- getFilePath
    liftIO $ do
      python <- Conf.require conf "language.python.interpreter"
      pytest <-  Conf.require conf "language.python.pytest"
      scripts <- Conf.require conf "language.python.scripts"
      sf1 <- getSafeExecConf (Conf.subconfig "safeexec" conf)
      sf2 <- getSafeExecConf (Conf.subconfig "language.python.safeexec" conf)
      let sf = sf2 `override` sf1
      let tstfile = getDoctest path page
      c <- doesFileExist tstfile
      if c then
          withTextTemp "tmp.py" code $ \pyfile ->
                 pythonResult <$>
                 safeExecWith sf python [pytest, scripts, tstfile, pyfile] ""
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
getDoctest :: FilePath -> Page -> FilePath
getDoctest filepath page
  = maybe
       (replaceExtension filepath ".tst")
       (takeDirectory filepath </>)
       (lookupFromMeta "doctest" $ pageMeta page)

  
