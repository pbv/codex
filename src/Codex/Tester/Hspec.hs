{-# LANGUAGE OverloadedStrings #-}
--------------------------------------------------------------------------
-- Test Haskell code using Hspec
--------------------------------------------------------------------------
module Codex.Tester.Hspec (
  hspecHaskellTester, hspecClangTester
  ) where

import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Data.Maybe (fromMaybe)
import           Data.Map.Strict (fromList)
import           Codex.Tester
import           Text.Pandoc.Definition (Meta(..), MetaValue(..))
import qualified Text.Pandoc.Builder as P
import           Control.Exception(handle)


-- | running and evaluating Haskell submissions
hspecHaskellTester :: Tester Result
hspecHaskellTester = tester "hspec" $ checkForbidden $ do
  Code lang src <- testCode
  guard (lang == "haskell")
  path <- testFilePath
  specPath <- fromMaybe (replaceExtension path ".hs") <$> metadataPath "spec"
  assert (fileExists specPath)
      ("spec file not found: " <> show specPath)
  spec <- liftIO $ T.readFile specPath
  let dir = takeDirectory path
  files <- globPatterns dir =<< metadataWithDefault "extra-files" []
  args <- (map T.unpack . getHspecArgs) <$> testMetadata
  ghc <- configured "language.haskell.compiler"
  profile <- configured "language.haskell.firejail"
  imports <- metadataWithDefault "imports" ""
  let code = imports <> "\n" <> src
  liftIO (haskellRunner profile ghc args files code spec)


-- | running and evaluaing C submissions
hspecClangTester :: Tester Result
hspecClangTester = tester "hspec" $ do
  Code lang src <- testCode
  guard (lang == "c")
  path <- testFilePath
  let dir = takeDirectory path
  specPath <- fromMaybe (replaceExtension path ".hs") <$> metadataPath "spec"
  assert (fileExists specPath)
      ("spec file not found: " <> show specPath)
  spec <- liftIO $ T.readFile specPath
  files <- globPatterns dir =<< metadataWithDefault "extra-files" []
  args <- (map T.unpack . getHspecArgs) <$> testMetadata
  ghc <- configured "language.haskell.compiler"
  gcc <- configured "language.c.compiler"
  profile <- configured "language.haskell.firejail"
  liftIO (clangRunner profile gcc ghc args files src spec)



getHspecArgs :: Meta -> [Text]
getHspecArgs 
  = getMetaArgs ["qc-max-success", "qc-max-size",
                 "qc-max-discard", "seed", "format", "depth"]
                ["ignore-dot-hspec", "fail-fast", "dry-run"] .
    (<>defaultMeta)

defaultMeta :: Meta
defaultMeta = Meta $ fromList [ -- ("format", MetaString "failed-examples")
                               ("ignore-dot-hspec", MetaBool True)
                              ]

haskellRunner ::
  FilePath -> FilePath -> [String] -> [FilePath] -> Text -> Text
  -> IO Result
haskellRunner profile ghc qcArgs files code props =
   withTempDir "codex" $ \dir -> handle compileErrorHandler $ do
     copyFiles files dir
     let hsFile   = dir </> "Submission.hs"
     let mainFile = dir </> "Main.hs"
     let exeFile = dir </> "Main"
     cmd:args <- parseArgs ghc
     let args' = args ++ ["-i"++dir, mainFile, "-o", exeFile]
     T.writeFile hsFile (header <> code)
     T.writeFile mainFile props
     runProcess profile cmd args'
     classify <$> sandboxExec profile exeFile (Just dir) qcArgs ""

header :: Text
header = "module Submission where\n\n"

clangRunner ::
  FilePath -> String -> String -> [String] -> [FilePath] -> Text -> Text
  -> IO Result
clangRunner profile gcc_cmd ghc_cmd qcArgs files c_code props =
  withTempDir "codex" $ \dir -> handle compileErrorHandler $ do
      copyFiles files dir       -- copy extra files
      let cFile  = dir </> "submit.c"
      let hsFile = dir </> "Main.hs"
      let objFile = dir </> "submit.o"
      let exeFile = dir </> "Main"
      T.writeFile cFile c_code
      T.writeFile hsFile props
      gcc:cc_args <- parseArgs gcc_cmd
      ghc:hc_args <- parseArgs ghc_cmd
      let cc_args'= cc_args ++ ["-c", cFile, "-o",  objFile]
      let hc_args'= hc_args ++ ["-i"++dir, objFile, hsFile, "-o",  exeFile]
      -- compile C code to object file
      runProcess profile gcc cc_args'
      -- compile Haskell quickcheck driver
      runProcess profile ghc hc_args'
      -- execute and under sandbox and classify result
      classify <$> sandboxExec profile exeFile (Just dir) qcArgs ""

classify :: ProcessRun -> Result
classify (ProcessRun ExitSuccess stdout _)  = accepted (P.codeBlock stdout)
classify (ProcessRun (ExitFailure _) stdout stderr)
  | match "TimeLimitExceeded" stderr = timeLimitExceeded (P.codeBlock msg)
  | match "out of memory" stderr = memoryLimitExceeded (P.codeBlock msg)
  | match "Command terminated by signal" stderr
                                = runtimeError (P.codeBlock msg)
  | match "Failed!" stdout       = wrongAnswer (P.codeBlock stdout) 
  | match "Failures:" stdout     = wrongAnswer (P.codeBlock (stdout<>stderr))
  | match "Exception" stdout     = runtimeError (P.codeBlock stdout)
  | match "Command exited with non-zero status" stderr
                                 = runtimeError (P.codeBlock msg)
  | otherwise                    = miscError (P.codeBlock msg)
  where msg = stdout <> stderr



    
    
