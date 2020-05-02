{-# LANGUAGE OverloadedStrings #-}
--------------------------------------------------------------------------
-- Test Haskell code using QuickCheck
--------------------------------------------------------------------------
module Codex.Tester.HaskellQC (
  haskellQCTester
  ) where


import           Data.Text (Text)
import qualified Data.Text.IO as T
import           Data.Maybe (fromMaybe)
import           Codex.Tester 

import           Control.Exception (catch)
import           System.Directory(copyFile)



-- | running and evaluating Haskell submissions
haskellQCTester :: Tester Result
haskellQCTester = tester "quickcheck" $ do
  Code lang src <- testCode
  guard (lang == "haskell")
  path <- testFilePath
  let dir = takeDirectory path  
  qcpath <- fromMaybe (replaceExtension path ".hs")
            <$> metadataPath "properties"
  assert (fileExists qcpath)
      ("properties file not found: " <> show qcpath)
  props <- liftIO $ T.readFile qcpath
  files <- globPatterns dir =<< metadataWithDefault "files" []
  qcArgs <- getQuickCheckArgs <$> testMetadata
  ghc <- configured "language.haskell.compiler"
  limits <- configLimits "language.haskell.limits"
  liftIO (haskellRunner limits ghc qcArgs files src props `catch` return)


haskellRunner :: Limits -> FilePath -> [String] -> [FilePath]
              -> Text -> Text -> IO Result
haskellRunner limits ghc qcArgs files code props =
   withTempDir "codex" $ \tmpdir -> do
     -- copy extra files
     mapM_ (\f -> copyFile f (tmpdir </> takeFileName f)) files
     let hs_file   = tmpdir </> "Submission.hs"
     let main_file = tmpdir </> "Main.hs"
     let exe_file = tmpdir </> "Main"
     cmd:args <- parseArgs ghc
     let args' = args ++ ["-i"++tmpdir, main_file, "-o", exe_file]
     T.writeFile hs_file (header <> code)
     T.writeFile main_file props
     chmod executable tmpdir
     chmod writeable tmpdir
     chmod readable hs_file
     runCompiler (Just limits) cmd args'
     classify <$> safeExec limits exe_file Nothing qcArgs ""

header :: Text
header = "module Submission where\n\n"


classify :: (ExitCode, Text, Text) -> Result
classify (ExitSuccess, stdout, _) = accepted stdout
classify (ExitFailure _, stdout, stderr)
  | match "Time Limit" stderr      = timeLimitExceeded stderr
  | match "Memory Limit" stderr    = memoryLimitExceeded stderr
  | match "Exception" stdout       = runtimeError stdout
  | match "Failed!" stdout         = wrongAnswer stdout
  | otherwise                      = miscError (stdout<>stderr)

