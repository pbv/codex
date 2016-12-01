{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Language.Python(
  pythonTester
  ) where

import           Control.Applicative
import           Control.Monad (liftM2)
import           Control.Monad.IO.Class
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as T

import           Snap.Core(pass)
import           Snap.Snaplet(getSnapletUserConfig)

import           Language.Types
import           Markdown
import           Application
import           Page
import           Tester
import           Config
import           SafeExec

import           System.Exit
import           System.FilePath
import           System.Directory


import qualified Data.Configurator as Configurator

type Doctest = FilePath   -- path to doctest script

-- get the doctest path for a problem
getDoctest :: Page -> Doctest
getDoctest Page{..}
  = root </> (maybe
              (replaceExtension path ".tst")
              (takeDirectory path </>)
              (lookupFromMeta "doctest" meta))


-- running and evaluating python submissions
pythonTester :: Page -> Code -> Codex Result
pythonTester page (Code (Language "python") code) = do
    conf <- getSnapletUserConfig
    python <- liftIO $ Configurator.require conf "language.python.interpreter"
    sf <- liftIO $ liftM2 (<>)
          (getSafeExecConf "language.python.safeexec" conf)
          (getSafeExecConf "safeexec" conf)
    let tstfile = getDoctest page
    liftIO $ do
      c <- doesFileExist tstfile
      if c then pythonTesterIO sf python code tstfile
        else return (miscError $ T.pack $ "missing doctest file: " ++ tstfile)
pythonTester _ _ = pass             


pythonTesterIO :: SafeExecConf -> FilePath -> Text -> Doctest -> IO Result
pythonTesterIO sf python code tstfile  = 
    withTextTemp "tmp.py" code $ \pyfile ->
    pythonResult <$>
      (safeExecWith sf python ["python/pytest.py", tstfile, pyfile] "")


pythonResult :: (ExitCode, Text, Text) -> Result
pythonResult (exitCode, stdout, stderr) 
  | T.null stdout && match "OK" stderr = accepted stderr
  | match "Time Limit" stderr          = timeLimitExceeded stderr
  | match "Memory Limit" stderr        = memoryLimitExceeded stderr
  | match "Exception Raised" stdout    = runtimeError stdout
  | match "SyntaxError" stderr         = compileError stderr
  | match "Failed" stdout              = wrongAnswer stdout
  | otherwise                   = miscError (stdout `T.append` stderr)
