--------------------------------------------------------------------------
-- Test C code using Haskell QuickCheck
--------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

module Codex.Tester.C (
  clangQCTester
  ) where

import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import           System.FilePath
import           System.Exit

import           Control.Exception

import           Codex.Tester
import           Codex.Tester.QuickCheck


clangQCTester :: FilePath -> Meta -> Code -> Test Result
clangQCTester path meta (Code lang src) = do
  guard (lang == "c")
  let base = takeDirectory path
  case getQuickCheckPath base meta of
    Nothing -> return (miscError "no QuickCheck file specified")
    Just qcpath -> do
      props <- liftIO $ T.readFile qcpath
      let qcArgs = getQuickCheckArgs meta
      -- add optional header to user submission
      let code = case getHeader meta of
                    Nothing -> src
                    Just header -> header `T.append` src
      ghc <- configured "language.haskell.compiler"
      gcc <- configured "language.c.compiler"
      limits <- getLimits "language.haskell.limits"
      liftIO (clangRunner limits gcc ghc qcArgs code props `catch` return)

clangRunner ::
  Limits -> String -> String -> [String] -> Text -> Text -> IO Result
clangRunner limits gcc_cmd ghc_cmd qcArgs c_code props =
  withTextTemp "sub.c" c_code $ \c_file ->
  withTextTemp "Main.hs" props $ \hs_file ->
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
       haskellResult <$> safeExecIO limits out_file qcArgs "")
   (cleanupFiles temps)


haskellResult :: (ExitCode, Text, Text) -> Result
haskellResult (_, stdout, stderr)
  | match "Not in scope" stderr ||
    match "parse error" stderr  ||
    match "Couldn't match" stderr  = compileError stderr
  | match "Time Limit" stderr   = timeLimitExceeded stdouterr
  | match "Memory Limit" stderr = memoryLimitExceeded stdouterr
  | match "Command terminated by signal" stderr
                                = runtimeError stdouterr
  | match "Failed" stdout       = wrongAnswer stdout 
  | match "Command exited with non-zero status" stderr
                                = runtimeError stdouterr
  | match "OK" stdout           = accepted stdout
  | otherwise                  = miscError stdouterr
  where stdouterr = stdout `T.append` stderr


-- get optional C declarations from a page
getHeader :: Meta -> Maybe Text
getHeader = lookupFromMeta "header" 

