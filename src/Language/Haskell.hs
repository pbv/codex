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

import           Test.QuickCheck (Args(..), stdArgs)
  

haskellTester :: Page -> Code -> AppHandler Result
haskellTester page (Code (Just Haskell) code) = do
    hsConf <- gets haskellConf
    let path = getQuickcheckPath page
    let args = getQuickcheckArgs page
    liftIO $ do
      c <- doesFileExist path
      if c then
        T.readFile path >>= haskellTesterIO hsConf args code 
        else return (miscError $ T.pack $
                     "missing QuickCheck file: " ++ path)
haskellTester _ _  = pass             



-- get the filepath to Quickcheck properties
-- NB: properties-only (not a module)
getQuickcheckPath :: Page -> FilePath
getQuickcheckPath Page{..} 
  = root </>
    fromMaybe (replaceExtension path ".hs") (lookupFromMeta "quickcheck" meta)


getQuickcheckArgs :: Page -> Args
getQuickcheckArgs Page{..} =
  let success = fromMaybe (maxSuccess stdArgs) $
                lookupFromMeta "maxSuccess" meta
      size = fromMaybe (maxSize stdArgs) $
             lookupFromMeta "maxSize" meta
      discard = fromMaybe (maxDiscardRatio stdArgs) $
                lookupFromMeta "maxDiscardRatio" meta
  in stdArgs { maxSuccess = success,
               maxSize = size,
               maxDiscardRatio = discard
             }

     

haskellTesterIO :: HaskellConf -> Args -> Text -> Text -> IO Result
haskellTesterIO HaskellConf{..} args haskell props =
   withTempFile "Temp.hs" $ \(codefile, h) ->      
   let codemod = T.pack (takeBaseName codefile)
       dir = takeDirectory codefile
   in do
     T.hPutStrLn h (moduleHeader codemod)
     T.hPutStrLn h haskell
     hClose h
     withTextTemp "Main.hs" (testScript args codemod props) $ \tstfile -> 
       haskellResult <$>
       safeExecWith haskellSfConf haskellExec ["-i"++dir, tstfile] "" 



testScript :: Args -> Text -> Text -> Text
testScript args codemod props
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
      "main = $forAllProperties (quickCheckWithResult " <>
      T.pack (show args) <>
      ") >>= \\c -> if c then exitSuccess else exitFailure"
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


