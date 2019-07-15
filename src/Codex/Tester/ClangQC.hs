--------------------------------------------------------------------------
-- Test C code using Haskell QuickCheck
--------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

module Codex.Tester.ClangQC (
  clangQCTester
  ) where

import           Data.Text (Text)
import qualified Data.Text.IO as T
import           Data.Maybe (fromMaybe)

import           Control.Exception (catch)
import           Codex.Tester


clangQCTester :: Tester Result
clangQCTester = tester "quickcheck" $ do
  Code lang src <- askSubmitted
  guard (lang == "c")
  path <- askPath
  qcpath <- fromMaybe (replaceExtension path ".hs")
            <$>
            metadataPath "properties"
  assert (fileExists qcpath)
    ("properties file not found: " <> show qcpath)
  props  <- liftIO (T.readFile qcpath)
  ghc    <- configured "language.haskell.compiler"
  gcc    <- configured "language.c.compiler"
  limits <- askLimits "language.haskell.limits"
  qcArgs <- getQuickCheckArgs <$> askMetadata
  -- append an optional header (for includes, prototypes, etc.)
  header <- fromMaybe "" <$> metadata "header"
  let code = header <> "\n" <> src
  liftIO (clangRunner limits gcc ghc qcArgs code props `catch` return)

clangRunner :: Limits
            -> String
            -> String
            -> [String]
            -> Text
            -> Text -> IO Result
clangRunner limits gcc_cmd ghc_cmd qcArgs c_code props =
  withTempDir "codex" $ \dir -> do
      let c_file  = dir </> "submit.c"
      let hs_file = dir </> "Main.hs"
      let obj_file = dir </> "submit.o"
      let exe_file = dir </> "Main"
      T.writeFile c_file c_code
      T.writeFile hs_file props
      gcc:cc_args <- parseArgs gcc_cmd
      ghc:hc_args <- parseArgs ghc_cmd
      let cc_args'= cc_args ++ ["-c", c_file, "-o", obj_file]
      let hc_args'= hc_args ++ ["-i"++dir, obj_file, hs_file, "-o", exe_file]
      -- compile C code to object file
      runCompiler gcc cc_args'
      -- compile Haskell quickcheck driver
      runCompiler ghc hc_args'
      -- allow anyone to execute the binary (for safeExec)
      chmod executable dir
      chmod readable exe_file
      -- execute and under safeExec and classify result
      classify <$> safeExec limits exe_file Nothing qcArgs ""

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


