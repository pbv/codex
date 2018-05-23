--------------------------------------------------------------------------
-- Test C code using input/output files
--------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

module Codex.Tester.InputOutput (
  clangIOTester
  ) where

import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Data.Maybe (fromMaybe)
import           Data.Function(on)
import           Data.List(maximumBy)

import           Control.Exception (catch, finally)

import           Codex.Tester


clangIOTester :: PageInfo -> Code -> Test Result
clangIOTester (PageInfo path meta) (Code lang src) = do
  guard (lang == "c")
  guard (tester meta == Just "stdio")
  ----
  let dir = takeDirectory path
  let inputs  = map (dir</>) $ fromMaybe [] (lookupFromMeta "inputs" meta) 
  let outputs = map (dir</>) $ fromMaybe [] (lookupFromMeta "outputs" meta)
  assert (pure $ length inputs > 0 && length outputs > 0)
    "inputs or output lists undefined"
  assert (pure $ length inputs == length outputs)
    "input and output lists should have equal length"   
  gcc <- configured "language.c.compiler"
  limits <- getLimits "language.c.limits"
  liftIO (ioRunner limits gcc src inputs outputs `catch` return)

ioRunner :: Limits -> String -> Text -> [FilePath] -> [FilePath] -> IO Result
ioRunner limits gcc_cmd code inputs outputs =
  withTemp "submit.c" code $ \c_file -> do
    let dir = takeDirectory c_file
    let exe_file = dir </> takeBaseName c_file <.> "exe"
    let gcc:cc_args = words gcc_cmd
    finally
      (do runCompiler gcc (cc_args  ++ [c_file, "-o", exe_file, "-lm"])
          runIOs limits exe_file inputs outputs
      ) (cleanupFiles [exe_file])

runIOs limits exe_file inputs outputs = do
  aggregate <$> zipWithM (runIO limits exe_file) inputs outputs

runIO limits exe_file in_file out_file = do
  in_txt <- T.readFile in_file
  out_txt <- T.readFile out_file
  r <- safeExec limits exe_file [] in_txt
  return (classify r in_txt out_txt)

-- | aggregate all results into a single one
-- used ordering to choose the most servere result
aggregate :: [Result] -> Result
aggregate rs
  = maximumBy (compare`on`resultClassify)  (rs ++ [accepted msg])
 where
   msg = "OK, passed " <> T.pack (show $ length rs) <> " tests"



classify :: (ExitCode, Text, Text) -> Text -> Text -> Result
classify (ExitFailure c, out, err) _ _
  = runtimeError $
    "Program exited with non-zero status: " <> T.pack (show c) <> "\n" <> err
classify (_, out, err) _ _
  | match "Command terminated by signal" err ||
    match "Command exited with non-zero status" err
  = runtimeError (out <> err)
classify (_, out, _) _ expected
  | out == expected
  = accepted "Test passed"
classify (_, out, _) input expected
  = (if T.strip out == T.strip expected
     then
       presentationError else wrongAnswer) $
    T.unlines ["Input:", input, "Expected output:", expected,
                "Obtained output:", out]
