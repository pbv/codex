--------------------------------------------------------------------------
-- Test C code using Haskell QuickCheck
--------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

module Codex.Tester.C (
  clangTester
  ) where

import           Control.Monad.State
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import           System.FilePath
import           System.Process.Text
import           System.Exit

import           Control.Exception

import qualified Data.Configurator as Conf

import           Codex.Tester.QuickCheck
import           Codex.Tester
import           Codex.SafeExec


clangTester :: Tester Result
clangTester = language "c" $ \code -> do
  conf <- getConfig
  page <- getPage
  base <- takeDirectory <$> getFilePath
  liftIO $ do
    ghc <- Conf.require conf "language.haskell.compiler"
    gcc <- Conf.require conf "language.c.compiler"
    sf1 <- getSafeExecConf (Conf.subconfig "safeexec" conf)
    sf2 <- getSafeExecConf (Conf.subconfig "language.haskell.safeexec" conf)
    let sf = sf2 `override` sf1
    case getQuickcheckPath base page of
      Nothing -> return (miscError "no QuickCheck file specified")
      Just qcpath -> do
        let args = getQuickcheckArgs page
        props <- T.readFile qcpath
        clangRunner sf gcc ghc args code props `catch` return


clangRunner sf gcc_cmd ghc_cmd qcArgs c_code props =
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



runCompiler :: FilePath -> [String] -> IO ()
runCompiler cmd args = do
  (exitCode, _, err) <- readProcessWithExitCode cmd args ""
  case exitCode of
    ExitFailure _ ->
      throw (compileError err)
    ExitSuccess ->
      return ()


testScript :: Text -> Text
testScript props
  = T.unlines
    [ "{-# LANGUAGE TemplateHaskell #-}",
      "module Main where",
      "import System.Exit",
      "import System.Environment(getArgs)",
      "import Codex.QuickCheck",
      "",
      props,
      "",
      "return []",
      "main = do qcArgs<-fmap (read.head) getArgs; $forAllProperties (quickCheckWithResult qcArgs) >>= \\c -> if c then exitSuccess else exitFailure"
    ]


haskellResult :: (ExitCode, Text,Text) -> Result
haskellResult (_, stdout, stderr)
  | match "Not in scope" stderr ||
    match "parse error" stderr  ||
    match "Couldn't match" stderr  = compileError stderr
  | match "Time Limit" stderr   = timeLimitExceeded stderr
  | match "Memory Limit" stderr = memoryLimitExceeded stderr
  | match "Failed" stdout       = wrongAnswer stdout
  | match "Command terminated by signal" stderr  = runtimeError stderr
  | match "Command exited with non-zero status" stderr = runtimeError stderr
  | otherwise     = accepted stdout
