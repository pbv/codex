--------------------------------------------------------------------------
-- Test Haskell code using QuickCheck
--------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

module Codex.Tester.Haskell (
  haskellQCTester
  ) where


import           Data.Text (Text)
import qualified Data.Text.IO as T

import           Codex.Tester 

import           Control.Exception (catch)



-- | running and evaluating Haskell submissions
haskellQCTester :: PageInfo -> Code -> Test Result
haskellQCTester (PageInfo path meta) (Code lang src) = do
  guard (lang == "haskell")
  guard (tester meta == Just "quickcheck")
  --------------
  let qcpath = replaceExtension path ".hs"
  assert (fileExists qcpath)
    ("quickcheck file not found: " <> show qcpath)
  props <- liftIO $ T.readFile qcpath
  let qcArgs = getQuickCheckArgs meta
  ghc <- configured "language.haskell.compiler"
  limits <- getLimits "language.haskell.limits"
  liftIO (haskellRunner limits ghc qcArgs src props `catch` return)


haskellRunner :: Limits -> FilePath -> [String] -> Text -> Text -> IO Result
haskellRunner limits ghc qcArgs code props =
   withTempDir "codex" $ \dir -> do
     let hs_file   = dir </> "Submission.hs"
     let main_file = dir </> "Main.hs"
     let exe_file = dir </> "Main"
     let cmd:args = words ghc
     let args' = args ++ ["-i"++dir, main_file, "-o", exe_file]
     T.writeFile hs_file (header <> code)
     T.writeFile main_file props
     runCompiler cmd args'
     chmod executable dir
     chmod readable exe_file
     classify <$> safeExec limits exe_file qcArgs ""

header :: Text
header = "module Submission where\n\n"


classify :: (ExitCode, Text, Text) -> Result
classify (_, stdout, stderr)
  | match "Not in scope" stderr ||
    match "parse error" stderr  ||
    match "Couldn't match" stderr  = compileError stderr
  | match "Time Limit" stderr   = timeLimitExceeded stderr
  | match "Memory Limit" stderr = memoryLimitExceeded stderr
  | match "Failed" stdout       = wrongAnswer stdout
  | match "Command exited with non-zero status" stderr = miscError stderr
  | match "OK" stdout = accepted stdout
  | otherwise  = miscError (stdout <> stderr)

