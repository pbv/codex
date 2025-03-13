
{-# LANGUAGE OverloadedStrings #-}
--------------------------------------------------------------------------
-- Test Haskell code using Hspec
--------------------------------------------------------------------------
module Codex.Tester.Hspec (
  hspecTester
  ) where

import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Data.Maybe (fromMaybe)
import           Data.Map.Strict (fromList)
import           Codex.Tester
import           Text.Pandoc.Definition (Meta(..), MetaValue(..))
import           Control.Exception(handle)
import           System.Directory(copyFile)


-- | running and evaluating Haskell submissions
hspecTester :: Tester Result
hspecTester = tester "hspec" $ do
  Code lang src <- testCode
  guard (lang == "haskell")
  path <- testFilePath
  let dir = takeDirectory path
  specPath <- fromMaybe (replaceExtension path ".hs") <$> metadataPath "spec"
  assert (fileExists specPath)
      ("spec file not found: " <> show specPath)
  spec <- liftIO $ T.readFile specPath
  files <- globPatterns dir =<< metadataWithDefault "files" []
  args <- (map T.unpack . getHspecArgs) <$> testMetadata
  ghc <- configured "language.haskell.compiler"
  limits <- configLimits "language.haskell.limits"
  liftIO (haskellRunner limits ghc args files src spec)

getHspecArgs :: Meta -> [Text]
getHspecArgs 
  = getMetaArgs ["qc-max-success", "qc-max-size",
                 "qc-max-discard", "seed", "format", "depth"]
                ["ignore-dot-hspec", "fail-fast", "dry-run"] .
    (<>defaultMeta)

defaultMeta = Meta $ fromList [ -- ("format", MetaString "failed-examples")
                               ("ignore-dot-hspec", MetaBool True)
                              ]

haskellRunner :: Limits -> FilePath -> [String] -> [FilePath] -> Text -> Text
              -> IO Result
haskellRunner limits ghc qcArgs files code props =
   withTempDir "codex" $ \dir -> handle compileErrorHandler $ do
     -- copy extra files
     mapM_ (\f -> copyFile f (dir </> takeFileName f)) files
     let hs_file   = dir </> "Submission.hs"
     let main_file = dir </> "Main.hs"
     let exe_file = dir </> "Main"
     cmd:args <- parseArgs ghc
     let args' = args ++ ["-i"++dir, main_file, "-o", exe_file]
     T.writeFile hs_file (header <> code)
     T.writeFile main_file props
     chmod executable dir
     chmod writeable dir
     chmod readable hs_file
     runProcess (Just limits) cmd args'
     classify <$> safeExec limits exe_file Nothing qcArgs ""

header :: Text
header = "module Submission where\n\n"


classify :: (ExitCode, Text, Text) -> Result
classify (ExitSuccess, stdout, _) = accepted stdout
classify (ExitFailure _, stdout, stderr)
  | match "Time Limit" stderr      = timeLimitExceeded stderr
  | match "Memory Limit" stderr    = memoryLimitExceeded stderr
  | match "Exception" stdout       = runtimeError stdout
  | match "Failures:" stdout       = wrongAnswer (stdout<>stderr)
  | otherwise                      = miscError (stdout<>stderr)

