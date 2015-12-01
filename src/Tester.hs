{-# LANGUAGE RecordWildCards, OverloadedStrings, DeriveDataTypeable #-}
module Tester where

import           Control.Applicative
import           Control.Monad.Trans
import           Control.Monad.State
import           Data.Typeable

import           Data.Monoid
import           Data.Text(Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           System.Exit
import           System.FilePath
import           System.IO (hClose)
--import           System.Directory

import qualified Data.ByteString.Char8 as B
import           Snap.Core(logError)

import           Types
import           Application
import           SafeExec
import           Language
import           Utils

data Result = Accepted
            | WrongAnswer
            | CompileError
            | RuntimeError
            | TimeLimitExceeded
            | MemoryLimitExceeded
            | MiscError
              deriving (Eq, Read, Show, Typeable)


pythonTester :: Tests -> Code -> AppHandler (Result,Text)
pythonTester doctest python = do
    pyConf <- gets pythonConf
    sfConf <- gets safeExecConf
    liftIO $ pythonTesterIO pyConf sfConf doctest python
  
pythonTesterIO ::  PythonConf
                   -> SafeExecConf
                   -> Tests 
                   -> Code
                   -> IO (Result, Text)
pythonTesterIO PythonConf{..} sfConf (Tests doctest) (Code python) = 
    withTextTemp "tmp.py" python $ \pyfile ->
    withTextTemp "tmp.tst" doctest $ \tstfile ->
    (pythonResult <$>
     safeExecWith sfConf pythonExec [pythonScript, tstfile, pyfile] "")
    
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


--------------------------------------------------------------------------

haskellTester :: Tests -> Code -> AppHandler (Result,Text)
haskellTester quickcheck haskell = do
    hsConf <- gets haskellConf
    sfConf <- gets safeExecConf
    liftIO $ haskellTesterIO hsConf sfConf quickcheck haskell

haskellTesterIO :: HaskellConf
                   -> SafeExecConf
                   -> Tests 
                   -> Code
                   -> IO (Result,Text)
haskellTesterIO HaskellConf{..} sfConf (Tests props) (Code code) = 
   withTempFile "Temp.hs" $ \(codefile, h) ->
   let codemod = T.pack $ takeBaseName codefile
       dir = takeDirectory codefile
   in do T.hPutStrLn h (moduleHeader codemod)
         T.hPutStrLn h code
         hClose h
         withTextTemp "Main.hs" (testScript codemod props) $ \tstfile -> 
           safeExecWith sfConf haskellExec ["-i"++dir, tstfile] "" >>=
           (return . haskellResult)



testScript :: Text -> Text -> Text
testScript codemod props
  = T.unlines
    [languageHeader "TemplateHaskell",
     moduleHeader "Main",
     importHeader "System.Exit",
     importHeader "Test.QuickCheck",
     importHeader "Test.QuickCheck.All",
     importHeader "Test.QuickCheck.Function",
     importHeader codemod,
     "",
     props,
     "",
     "return []",
     "main = $quickCheckAll >>= \\c -> if c then exitSuccess else exitFailure"
    ]
    

moduleHeader :: Text -> Text
moduleHeader name = "module " <> name <> " where"

importHeader :: Text -> Text
importHeader mod = "import "<>  mod 

languageHeader :: Text -> Text
languageHeader ext =  "{-# LANGUAGE " <> ext <> "#-}"

{-
  putStr "exitCode=" >> print exitCode
  putStrLn "stdout=" >> putStrLn (T.unpack stdout)
  putStrLn "stderr=" >> putStrLn (T.unpack stderr)  
  return (result,msg)
-}

haskellResult (exitCode, stdout, stderr)  
  | match "Not in scope" stderr ||
    match "parse error" stderr  ||
    match "Couldn't match" stderr  = (CompileError, stderr)
  | match "Time Limit" stderr   = (TimeLimitExceeded, stderr)
  | match "Memory Limit" stderr = (MemoryLimitExceeded, stderr)
  | match "Failed" stdout       = (WrongAnswer, stdout)
  | match "Command exited with non-zero status" stderr = (MiscError, stderr)
  | otherwise = (Accepted, stdout)


match :: Text -> Text -> Bool
match = T.isInfixOf

-- trim a text to a maximum length
trim :: Int -> Text -> Text
trim size txt
  | T.length txt <= size = txt
  | otherwise = T.append (T.take size txt) "\n**Output too long (truncated)***\n"

  
