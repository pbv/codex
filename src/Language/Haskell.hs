--------------------------------------------------------------------------
-- Test Haskell code using QuickCheck
--------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Language.Haskell (
  haskellTester
  ) where

import           Control.Applicative
import           Control.Monad.State
import           Data.String
import           Data.Maybe
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Data.Monoid

import           System.FilePath
import           System.Directory
import           System.IO

import           Snap.Core(pass)

import           Types
import           Language.Types
import           Markdown
import           Tester
import           Application
import           Problem
import           SafeExec


  

haskellTester :: Page -> Code -> AppHandler Result
haskellTester page (Code Haskell code) = do
    hsConf <- gets haskellConf
    let propfile = getProperties page
    liftIO $ do
      c <- doesFileExist propfile
      if c then
        T.readFile propfile >>= haskellTesterIO hsConf code 
        else return (miscError $ T.pack $
                     "missing QuickCheck properties file: " ++ propfile)
haskellTester _ _ = pass             


type Properties = FilePath   -- NB: properties-only (not a module)

-- get the path to Quickcheck properties
getProperties :: Page -> Properties
getProperties Page{..} =
  fromMaybe (replaceExtension path ".hs") (lookupFromMeta "quickcheck" meta)
  

haskellTesterIO :: HaskellConf -> Text -> Text -> IO Result
haskellTesterIO HaskellConf{..} haskell props =
   withTempFile "Temp.hs" $ \(codefile, h) ->      
   let codemod = T.pack (takeBaseName codefile)
       dir = takeDirectory codefile
   in do
     T.hPutStrLn h (moduleHeader codemod)
     T.hPutStrLn h haskell
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
    match "Couldn't match" stderr  = compileError stderr
  | match "Time Limit" stderr   = timeLimitExceeded stderr
  | match "Memory Limit" stderr = memoryLimitExceeded stderr
  | match "Failed" stdout       = wrongAnswer stdout
  | match "Command exited with non-zero status" stderr = miscError stderr
  | otherwise = accepted stdout


