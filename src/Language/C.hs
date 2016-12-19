--------------------------------------------------------------------------
-- Test C code using Haskell QuickCheck
--------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

module Language.C (
  clangTester
  ) where

import           Control.Monad.State
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Data.Monoid

import           System.FilePath
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
clangTester page (Code (Language "c") code) = do
  conf <- getSnapletUserConfig
  ghc <- liftIO $ Configurator.require conf "language.haskell.compiler"
  gcc <- liftIO $ Configurator.require conf "language.c.compiler"
  sf <- liftIO $ liftM2 (<>)
        (getSafeExecConf "language.haskell.safeexec" conf)
        (getSafeExecConf "safeexec" conf)
  liftIO $ case getQuickcheckPath page of
    Nothing ->   return (miscError "no QuickCheck file specified")
    Just qcpath -> do
      let args = getQuickcheckArgs page
      props <- T.readFile (publicPath </> qcpath)
      clangTesterIO sf gcc ghc args code props `catch` return
clangTester _ _ = pass


clangTesterIO sf gcc_cmd ghc_cmd qcArgs c_code props =
  withTextTemp "sub.c" c_code $ \c_file ->
  withTextTemp "Main.hs" (testScript props) $ \hs_file ->
  let dir = takeDirectory c_file
      c_obj_file = dir </> takeBaseName c_file <.> "o"
      out_file = dir </> takeBaseName hs_file
      temps = [c_obj_file, out_file, out_file <.> "o", out_file <.> "hi"]
      gcc:cc_args' = words gcc_cmd
      ghc:hc_args' = words ghc_cmd
      cc_args = cc_args' ++ ["-fPIC", "-std=c99", "-c", c_file, "-o", c_obj_file]
      hc_args = hc_args' ++ ["-i"++dir, "-dynamic", "-O0",
                             c_obj_file, hs_file, "-o", out_file]
  in
   finally
   (do -- compile C code; this should be safe to run without safeExec
       runCompiler gcc cc_args
       --- compile Haskell test script
       runCompiler ghc hc_args
       -- run compiled script under safe exec
       haskellResult <$> safeExecWith sf out_file [show qcArgs] "")
   (cleanupFiles temps)



runCompiler cmd args = do
  (exitCode, _, stderr) <- readProcessWithExitCode cmd args ""
  case exitCode of
    ExitFailure _ ->
      throw (compileError stderr)
    ExitSuccess ->
      return ()


testScript :: Text -> Text
testScript props
  = T.unlines
    [ "{-# LANGUAGE TemplateHaskell #-}",
      "module Main where",
      "import System.Exit",
      "import System.Environment(getArgs)",
      "import Test.QuickCheck",
      "import Test.QuickCheck.Function",
      "import Test.QuickCheck.Random",
      "",
      props,
      "",
      "return []",
      "main = do qcArgs<-fmap (read.head) getArgs; $forAllProperties (quickCheckWithResult qcArgs) >>= \\c -> if c then exitSuccess else exitFailure"
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
