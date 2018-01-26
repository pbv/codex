--------------------------------------------------------------------------
-- Test Haskell code using QuickCheck
--------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

module Codex.Tester.Haskell (
  haskellTester
  ) where


import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import           Data.Monoid

import           System.FilePath
import           System.IO.Temp
import           Control.Exception

import           Codex.Types
import           Codex.Tester
import           Codex.Tester.QuickCheck



-- | running and evaluating Haskell submissions
haskellTester :: Code -> Test Result
haskellTester (Code language code) = do
  guard (language == "haskell")
  page <- testPage
  base <- takeDirectory <$> testPath
  case getQuickCheckPath base page of
    Nothing ->  return (miscError "no QuickCheck file specified")
    Just qcpath -> do
      props <- liftIO $ T.readFile qcpath
      let qcArgs = getQuickCheckArgs page
      ghc <- testConfig "language.haskell.compiler"
      safeExec <- testSafeExec ["language.haskell.limits", "limits"]
      liftIO (haskellRunner safeExec ghc qcArgs code props `catch` return)


haskellRunner :: SafeExec -> FilePath -> [String] -> Text -> Text -> IO Result
haskellRunner safeExec ghc qcArgs code props =
   withSystemTempDirectory "codex" $ \dir -> do
   let hs_file   = dir </> "Submission.hs"
   let main_file = dir </> "Main.hs"
   let out_file = dir </> "Main"
   let temps = [ hs_file, main_file, out_file,
                 replaceExtension hs_file ".hi",
                 replaceExtension hs_file ".o",
                 replaceExtension main_file ".o",
                 replaceExtension main_file ".hi"
               ]
   let cmd:args = words ghc
   let args' = args ++ ["-i"++dir, main_file, "-o", out_file]
   finally
     (do ensureFileExecutable dir
         T.writeFile hs_file (modHeader code)
         T.writeFile main_file props
         runCompiler cmd args'
         haskellResult <$> safeExec out_file qcArgs ""
     ) (cleanupFiles temps)


modHeader :: Text -> Text
modHeader txt = if match "module " txt then txt else header <> txt

header :: Text
header = "module Submission where\n"


haskellResult (exitCode, stdout, stderr)
  | match "Not in scope" stderr ||
    match "parse error" stderr  ||
    match "Couldn't match" stderr  = compileError stderr
  | match "Time Limit" stderr   = timeLimitExceeded stderr
  | match "Memory Limit" stderr = memoryLimitExceeded stderr
  | match "Failed" stdout       = wrongAnswer stdout
  | match "Command exited with non-zero status" stderr = miscError stderr
  | match "OK" stdout = accepted stdout
  | otherwise  = miscError (stdout `T.append` stderr)

