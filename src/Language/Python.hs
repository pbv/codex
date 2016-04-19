{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Language.Python(
   pythonTester
  ) where

import           Control.Applicative
import           Control.Monad.IO.Class
import           Control.Monad.State
import           Data.String
import           Data.Maybe
import           Data.Text (Text)
import qualified Data.Text as T

import           Snap.Core(pass)

import           Text.Pandoc hiding (Code)

import           Types
import           Language.Types
import           Markdown
import           Application
import           Problem
import           Tester
import           SafeExec

import           System.Exit
import           System.FilePath
import           System.Directory


pythonTester :: Page -> Code -> AppHandler Result
pythonTester Problem{..} (Code Python code) = do
    pyConf <- gets pythonConf
    let tstfile = getDoctest page
    liftIO $ do
      c <- doesFileExist tstfile
      if c then pythonTesterIO pyConf code tstfile
        else return (miscError $ T.pack $ "missing doctest file: " ++ tstfile)
pythonTester _ _ = pass             

type Doctest = FilePath   -- doctest script

-- get the doctest file for a problem
getDoctest :: Page -> Doctest
getDoctest Page{..} 
  = fromMaybe (replaceExtension path ".tst") (lookupFromMeta "doctest" meta)


pythonTesterIO ::  PythonConf -> Text -> Doctest -> IO Result
pythonTesterIO PythonConf{..} python tstfile  = 
    withTextTemp "tmp.py" python $ \pyfile ->
    pythonResult <$>
    safeExecWith pythonSfConf pythonExec [pythonScript, tstfile, pyfile] ""


pythonResult :: (ExitCode, Text, Text) -> Result
pythonResult (exitCode, stdout, stderr) 
  | T.null stdout && match "OK" stderr = accepted stderr
  | match "Time Limit" stderr          = timeLimitExceeded stderr
  | match "Memory Limit" stderr        = memoryLimitExceeded stderr
  | match "Exception Raised" stdout    = runtimeError stdout
  | match "SyntaxError" stderr         = compileError stdout
  | match "Failed" stdout              = wrongAnswer stdout
  | otherwise                   = miscError (T.append stdout stderr)
