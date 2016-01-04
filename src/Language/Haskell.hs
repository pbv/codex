{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Language.Haskell (
  haskellTester
  ) where

import           Control.Applicative
import           Control.Monad.State
import           Data.String
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Data.Monoid
import           Database.SQLite.Simple.ToField
import           Database.SQLite.Simple.FromField

import           System.FilePath
import           System.Directory
import           System.IO

import           Types
import           Tester
import           Application
import           SafeExec




--------------------------------------------------------------------------
-- test haskell code with QuickCheck
--------------------------------------------------------------------------
--  | haskellTester :: Tests -> Code -> AppHandler (Result,Text)
haskellTester :: Tests -> Tester AppHandler 
haskellTester quickcheck haskell = do
    hsConf <- gets haskellConf
    liftIO $ haskellTesterIO hsConf quickcheck haskell


haskellTesterIO :: HaskellConf -> Tests -> Tester IO
haskellTesterIO HaskellConf{..} (Tests props) (Code code) = 
   withTempFile "Temp.hs" $ \(codefile, h) ->
   let codemod = T.pack (takeBaseName codefile)
       dir = takeDirectory codefile
   in do T.hPutStrLn h (moduleHeader codemod)
         T.hPutStrLn h code
         hClose h
         withTextTemp "Main.hs" (testScript codemod props) $ \tstfile -> 
           haskellResult <$>
           safeExecWith haskellSfConf haskellExec ["-i"++dir, tstfile] "" 



testScript :: Text -> Text -> Text
testScript codemod props
  = T.unlines
    [ "{-# LANGUAGE TemplateHaskell #-}",
      "module Main where",
      "import System.Exit",
      "import Test.QuickCheck",
      "import Test.QuickCheck.Function",
      "import " <> codemod,
      "",
      props,
      "",
      "return []",
      "main = $quickCheckAll >>= \\c -> if c then exitSuccess else exitFailure"
    ]
    

moduleHeader :: Text -> Text
moduleHeader name = "module " <> name <> " where"

haskellResult (exitCode, stdout, stderr)  
  | match "Not in scope" stderr ||
    match "parse error" stderr  ||
    match "Couldn't match" stderr  = (CompileError, stderr)
  | match "Time Limit" stderr   = (TimeLimitExceeded, stderr)
  | match "Memory Limit" stderr = (MemoryLimitExceeded, stderr)
  | match "Failed" stdout       = (WrongAnswer, stdout)
  | match "Command exited with non-zero status" stderr = (MiscError, stderr)
  | otherwise = (Accepted, stdout)


