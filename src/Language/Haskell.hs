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
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Data.Monoid

import           System.FilePath
import           System.Directory
import           System.IO

import           Snap.Core(pass)
import           Snap.Snaplet(getSnapletUserConfig)

import           Language.Types
import           Language.QuickCheck
import           Tester
import           Application
import           Page
import           SafeExec
import           Config

import           System.Directory
import           System.Process.Text
import           System.Exit
import           Control.Exception


import qualified Data.Configurator as Configurator


-- running and evaluating Haskell submissions

haskellTester :: Page -> Code -> Codex Result
haskellTester page (Code (Language "haskell") code) = do
    conf <- getSnapletUserConfig
    ghc <- liftIO $ Configurator.require conf "haskell.compiler"
    sf <- liftIO $ getSafeExecConf "haskell.safeexec" conf
    sf' <- liftIO $ getSafeExecConf "safeexec" conf
    let path = getQuickcheckPath page
    let args = getQuickcheckArgs page
    props <- liftIO $ T.readFile path
    liftIO (haskellTesterIO (sf<>sf') ghc args code props
            `catch` return)
haskellTester _ _  = pass             

     

haskellTesterIO ::
  SafeExecConf -> FilePath -> QuickCheckArgs -> Text -> Text -> IO Result
haskellTesterIO sf ghc args code props =
   withTempFile "Temp.hs" $ \(hs_file, h) ->      
   let codemod = T.pack $ takeBaseName hs_file
       dir = takeDirectory hs_file
   in do
     T.hPutStrLn h (moduleHeader codemod)
     T.hPutStrLn h code
     hClose h
     withTextTemp "Main.hs" (testScript args codemod props) $ \tstfile -> do
       let out_file = dir </> takeBaseName tstfile
       runCompiler ghc  ["-i"++dir, "-O0", "-dynamic", tstfile, "-o", out_file] 
       r <- haskellResult <$> safeExecWith sf out_file [] ""
       removeFile out_file
       return r



runCompiler cmd args = do
  (exitCode, _, stderr) <- readProcessWithExitCode cmd args ""
  case exitCode of
    ExitFailure _ ->
      throw (compileError stderr)
    ExitSuccess ->
      return ()



testScript :: QuickCheckArgs -> Text -> Text -> Text
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
      "stdArgs { maxSuccess = " <> T.pack (show $ maxSuccess args) <>
      ", maxSize = " <> T.pack (show $ maxSize args) <>
      ", maxDiscardRatio = " <> T.pack (show $ maxDiscardRatio args) <>
      "}) >>= \\c -> if c then exitSuccess else exitFailure"
    ]


moduleHeader :: Text -> Text
moduleHeader name
  = T.unlines ["{-# LANGUAGE Safe #-}", "module " <> name <> " where"]

haskellResult (exitCode, stdout, stderr)  
  | match "Not in scope" stderr ||
    match "parse error" stderr  ||
    match "Couldn't match" stderr  = compileError stderr
  | match "Time Limit" stderr   = timeLimitExceeded stderr
  | match "Memory Limit" stderr = memoryLimitExceeded stderr
  | match "Failed" stdout       = wrongAnswer stdout
  | match "Command exited with non-zero status" stderr = miscError stderr
  | otherwise = accepted stdout


