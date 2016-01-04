{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Language.Python(
  pythonTester
  ) where

import           Control.Applicative
import           Control.Monad.State
import           Data.String
import           Data.Text (Text)
import qualified Data.Text as T
import           Database.SQLite.Simple.ToField
import           Database.SQLite.Simple.FromField

import           Types
import           Application
import           Tester
import           SafeExec

import           System.Exit


pythonTester :: Tests -> Tester AppHandler 
pythonTester doctest python = do
    pyConf <- gets pythonConf
    liftIO $ pythonTesterIO pyConf doctest python
  
pythonTesterIO ::  PythonConf -> Tests -> Tester IO 
pythonTesterIO PythonConf{..} (Tests doctest) (Code python) = 
    withTextTemp "tmp.py" python $ \pyfile ->
    withTextTemp "tmp.tst" doctest $ \tstfile ->
    pythonResult <$>
    safeExecWith pythonSfConf pythonExec [pythonScript, tstfile, pyfile] ""


pythonResult :: (ExitCode, Text, Text) -> (Result, Text)
pythonResult (exitCode, stdout, stderr) = (result, trim maxLen msg)
  where
    maxLen = 2000
    (result, msg)
      | T.null stdout && match "OK" stderr = (Accepted, stderr)
      | match "Time Limit" stderr          = (TimeLimitExceeded, stderr)
      | match "Memory Limit" stderr        = (MemoryLimitExceeded, stderr)
      | match "Exception Raised" stdout    = (RuntimeError, stdout)
      | match "SyntaxError" stderr         = (CompileError, stdout)
      | match "Failed" stdout              = (WrongAnswer, stdout)
      | otherwise                          = (MiscError, T.append stdout stderr)

