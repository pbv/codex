--------------------------------------------------------------------------
-- Test C code using Haskell QuickCheck
--------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

module Codex.Tester.C (
  clangTester
  ) where

import           Data.Maybe (fromMaybe)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import           System.FilePath
import           System.Exit

import           Control.Exception

import           Codex.Page
import           Codex.Tester.QuickCheck
import           Codex.Tester


clangTester :: Tester Result
clangTester = withLanguage "c" $ \code -> do
  page <- testerPage
  base <- takeDirectory <$> testerPath
  case getQuickcheckPath base page of
    Nothing -> return (miscError "no QuickCheck file specified")
    Just qcpath -> do
      props <- liftIO $ T.readFile qcpath
      let args = getQuickcheckArgs page
      -- add optional header to user code
      let code' = case getHeader page of
                    Nothing -> code
                    Just header -> header `T.append` code
      ghc <- configured "language.haskell.compiler"
      gcc <- configured "language.c.compiler"
      limits <- testerLimits "language.c.limits"
      sf <- testerSafeExecPath
      liftIO (clangRunner sf limits gcc ghc args code' props `catch` return)


clangRunner sf limits gcc_cmd ghc_cmd qcArgs c_code props =
  withTextTemp "sub.c" c_code $ \c_file ->
  withTextTemp "Main.hs" (testScript props) $ \hs_file ->
  let dir = takeDirectory c_file
      c_obj_file = dir </> takeBaseName c_file <.> "o"
      out_file = dir </> takeBaseName hs_file
      temps = [c_obj_file, out_file, out_file <.> "o", out_file <.> "hi"]
      gcc:cc_args' = words gcc_cmd
      ghc:hc_args' = words ghc_cmd
      cc_args = cc_args' ++ ["-c", c_file, "-o", c_obj_file]
      hc_args = hc_args' ++ ["-i"++dir, c_obj_file, hs_file, "-o", out_file]
  in
   finally
   (do -- compile C code; this should be safe to run without safeExec
       runCompiler gcc cc_args
       --- compile Haskell test script
       runCompiler ghc hc_args
       -- run compiled script under safe exec
       haskellResult <$> safeExecWith sf limits out_file [show qcArgs] "")
   (cleanupFiles temps)




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
  | match "OK, passed" stdout   = accepted stdout
  | otherwise     = miscError (stdout `T.append` stderr)


-- get optional C declarations from a page
getHeader :: Page -> Maybe Text
getHeader = lookupFromMeta "header" . pageMeta

