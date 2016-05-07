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
import           Data.String
import           Data.Maybe
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.HashMap.Strict as HashMap
import           Data.Monoid

import           System.FilePath
import           System.Directory
import           System.IO
import           System.Process.Text
import           System.Exit

import           Snap.Core(pass)

import           Types
import           Language.Types
import           Language.QuickCheck
import           Markdown
import           Tester
import           Application
import           Page
import           SafeExec
import           Config
import           Control.Exception

import qualified Data.Configurator as Configurator


clangTester :: Page -> Code -> Codex Result
clangTester page (Code (Language "c_cpp") code) = do
  conf <- gets config
  ghc <- liftIO $ Configurator.require conf "haskell.compiler"
  gcc <- liftIO $ Configurator.require conf "c.compiler"
  sf <- liftIO $ getSafeExecConf "c." conf
  let path = getQuickcheckPath page
  let args = getQuickcheckArgs page
  props <- liftIO $ T.readFile path
  liftIO (clangTesterIO sf gcc ghc args code props `catch` return)
clangTester _ _ = pass


clangTesterIO sf gcc ghc args c_code props =
  withTempFile "foreign.c" $ \(c_file, h1) ->
  withTempFile "Main.hs" $ \(hs_file, h2) ->
  let dir = takeDirectory c_file
      obj_file = dir </> takeBaseName c_file <.> "o"
      out_file = dir </> takeBaseName hs_file
  in do
    -- create C code file
    T.hPutStrLn h1 c_code
    hClose h1
    -- create test script
    T.hPutStrLn h2 (testScript args props)
    hClose h2
    -- compile C code; this should be safe to run without safeExec
    runCompiler gcc ["-fPIC", "-std=c99", "-c", c_file, "-o", obj_file]
    --- compile Haskell test script
    runCompiler ghc ["-i"++dir, "-dynamic", "-O0",  obj_file, hs_file, "-o", out_file]
    -- run compiled script under safe exec
    r <- haskellResult <$> safeExecWith sf out_file [] ""
    -- cleanup the temporary object file
    removeFile obj_file
    removeFile out_file
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

    
