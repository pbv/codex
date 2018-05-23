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
import           Data.Maybe (fromMaybe)

import           Control.Exception (catch, finally)
import           Codex.Tester


clangQCTester :: PageInfo -> Code -> Test Result
clangQCTester (PageInfo path meta) (Code lang src) = do
  guard (lang == "c")
  guard (tester meta == Just "quickcheck")
  ----
  let qcpath = replaceExtension path ".hs"
  assert (fileExists qcpath)
    ("quickcheck file not found: " <> show qcpath)
  props <- liftIO (T.readFile qcpath)
  ghc <- configured "language.haskell.compiler"
  gcc <- configured "language.c.compiler"
  limits <- getLimits "language.haskell.limits"
  let qcArgs = getQuickCheckArgs meta
  -- optional code header 
  let code = fromMaybe "" (getHeader meta) <> src
  liftIO (clangRunner limits gcc ghc qcArgs code props `catch` return)

clangRunner ::
  Limits -> String -> String -> [String] -> Text -> Text -> IO Result
clangRunner limits gcc_cmd ghc_cmd qcArgs c_code props =
  withTemp "submit.c" c_code $ \c_file ->
  withTemp "Main.hs" props $ \hs_file ->
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
       classify <$> safeExec limits out_file qcArgs "")
   (cleanupFiles temps)


classify :: (ExitCode, Text, Text) -> Result
classify (_, stdout, stderr)
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
  where stdouterr = stdout <> stderr


-- get optional C declarations from a page
getHeader :: Meta -> Maybe Text
getHeader = lookupFromMeta "header" 

