{-# LANGUAGE RecordWildCards, OverloadedStrings, DeriveDataTypeable #-}
module Tester where

import           Control.Applicative
import           Control.Exception
import           Data.Typeable
import           Data.Text(Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           System.Exit
import           System.IO
import           System.Directory

import           Types
import           SafeExec
import           Language

data Result = Result { resultStatus :: !Status
                     , resultMsg :: !Text
                     } deriving (Eq, Show)


pythonTester :: PythonConf
                -> SafeExecConf
                -> Code Python
                -> Code Doctest
                -> IO Result
pythonTester PythonConf{..} safeConf (Code python) (Code doctest) 
  = withTextTemp "tmp.py" python $ \pyfile ->
    withTextTemp "tmp.tst" doctest $ \tstfile ->
    (pythonReport <$>
     safeExecWith safeConf  pythonExec [pythonScript, tstfile, pyfile] "")
    
pythonReport :: (ExitCode, Text, Text) -> Result
pythonReport (exitCode, stdout, stderr) = Result status msg
  where
    msg = trim maxLen stdout `T.append` trim maxLen stderr
    maxLen = 2000
    status
      | T.null stdout && match "OK" stderr = Accepted
      | match "Time Limit" stderr          = TimeLimitExceeded
      | match "Memory Limit" stderr        = MemoryLimitExceeded
      | match "Exception Raised" stdout    = RuntimeError
      | match "SyntaxError" stderr         = CompileError
      | match "Failed" stdout              = WrongAnswer
      | otherwise                          = MiscError

match = T.isInfixOf

          
-- haskellTester :: SafeExecConf -> Code Haskell -> Code QuickCheck -> IO Result
-- haskellTester = undefined

-- trim a text to a maximum length
trim :: Int -> Text -> Text
trim size txt
  | T.length txt <= size = txt
  | otherwise = T.append (T.take size txt) "\n**Output too long (truncated)***\n"


-- aquire and release a text temporary file
withTextTemp :: String -> Text -> (FilePath -> IO a) -> IO a
withTextTemp name txt k = bracket createTemp removeFile k
  where createTemp = do
          (file,handle) <- openTempFileWithDefaultPermissions tempDir name
          T.hPutStr handle txt
          hClose handle
          return file

tempDir :: FilePath
tempDir = "/tmp"
