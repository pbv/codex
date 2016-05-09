--------------------------------------------------------------------------
-- Test C code using Haskell QuickCheck
--------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Language.C (
  clangTester
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
import           System.Process.Text
import           System.Exit

import           Snap.Core(pass)
import           Snap.Snaplet(getSnapletUserConfig)

import           Language.Types
import           Language.QuickCheck

import           Tester
import           Application
import           Page
import           SafeExec
import           Config
import           Control.Exception

import qualified Data.Configurator as Configurator


clangTester :: Page -> Code -> Codex Result
clangTester page (Code (Language "c_cpp") code) = do
  conf <- getSnapletUserConfig
  ghc <- liftIO $ Configurator.require conf "haskell.compiler"
  gcc <- liftIO $ Configurator.require conf "c.compiler"
  sf <- liftIO $ getSafeExecConf "c.safeexec" conf
  sf' <- liftIO $ getSafeExecConf "haskell.safeexec" conf
  sf''<- liftIO $ getSafeExecConf "safeexec" conf
  let path = getQuickcheckPath page
  let args = getQuickcheckArgs page
  props <- liftIO $ T.readFile path
  liftIO (clangTesterIO (sf<>sf'<>sf'') gcc ghc args code props
          `catch` return)
clangTester _ _ = pass


clangTesterIO sf gcc ghc args c_code props =
  withTempFile "sub.c" $ \(c_file, h1) ->
  withTempFile "Main.hs" $ \(hs_file, h2) ->
  let dir = takeDirectory c_file
      c_obj_file = dir </> takeBaseName c_file <.> "o"
      out_file = dir </> takeBaseName hs_file
  in do
    -- create C code file
    T.hPutStrLn h1 c_code
    hClose h1
    -- create test script
    T.hPutStrLn h2 (testScript args props)
    hClose h2
    -- compile C code; this should be safe to run without safeExec
    runCompiler gcc ["-fPIC", "-std=c99", "-c", c_file, "-o", c_obj_file]
    --- compile Haskell test script
    runCompiler ghc ["-i"++dir, "-dynamic", "-O0",
                     c_obj_file, hs_file, "-o", out_file]
    -- run compiled script under safe exec
    r <- haskellResult <$> safeExecWith sf out_file [] ""
    -- cleanup temporary files
    mapM_ removeFile [c_obj_file, out_file, out_file <.> "o", out_file <.> "hi"]
    return r

runCompiler cmd args = do
  (exitCode, _, stderr) <- readProcessWithExitCode cmd args ""
  case exitCode of
    ExitFailure _ ->
      throw (compileError stderr)
    ExitSuccess ->
      return ()


testScript :: QuickCheckArgs -> Text -> Text
testScript args props
  = T.unlines
    [ "{-# LANGUAGE TemplateHaskell #-}",
      "module Main where",
      "import System.Exit",
      "import Test.QuickCheck",
      "import Test.QuickCheck.Function",
      "import Test.QuickCheck.Random",
      "",
      props,
      "",
      "return []",
      "main = $forAllProperties (quickCheckWithResult " 
      <> T.pack (setupArgs args) <>
      ") >>= \\c -> if c then exitSuccess else exitFailure"
    ]



haskellResult (exitCode, stdout, stderr)  
  | match "Not in scope" stderr ||
    match "parse error" stderr  ||
    match "Couldn't match" stderr  = compileError stderr
  | match "Time Limit" stderr   = timeLimitExceeded stderr
  | match "Memory Limit" stderr = memoryLimitExceeded stderr
  | match "Failed" stdout       = wrongAnswer stdout
  | match "Command terminated by signal" stderr  = runtimeError stderr
  | match "Command exited with non-zero status" stderr = runtimeError stderr
  | otherwise     = accepted stdout

    
