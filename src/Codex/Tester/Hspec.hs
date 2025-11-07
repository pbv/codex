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
import           Data.Maybe (fromMaybe, isJust)
import           Data.Map.Strict (fromList)
import           Codex.Tester
import           Text.Pandoc.Definition (Meta(..), MetaValue(..))
import qualified Text.Pandoc.Builder as P
import           Control.Exception(handle)
import           System.Directory(copyFile)
import           Text.Regex (mkRegex, matchRegex)

-- | running and evaluating Haskell submissions
hspecHaskellTester :: Tester Result
hspecHaskellTester = tester "hspec" $ do
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
  forbid <- (fmap mkRegex) <$> metadata "forbid"
  imports <- metadataWithDefault "imports" ""
  let code = imports <> "\n" <> src
  case forbid of
    Nothing ->
      liftIO (haskellRunner limits ghc args files code spec)
    Just re ->
      if isJust (matchRegex re (T.unpack src)) then
        return invalidSubmission
      else
        liftIO (haskellRunner limits ghc args files code spec)


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
  files <- globPatterns dir =<< metadataWithDefault "files" []
  args <- (map T.unpack . getHspecArgs) <$> testMetadata
  ghc <- configured "language.haskell.compiler"
  gcc <- configured "language.c.compiler"
  limits <- configLimits "language.haskell.limits"
  liftIO (clangRunner limits gcc ghc args files src spec)



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

clangRunner :: Limits -> String -> String -> [String] -> [FilePath] -> Text -> Text
            -> IO Result
clangRunner limits gcc_cmd ghc_cmd qcArgs files c_code props =
  withTempDir "codex" $ \dir -> handle compileErrorHandler $ do
      -- copy extra files
      mapM_ (\f -> copyFile f (dir </> takeFileName f)) files
      let c_file  = dir </> "submit.c"
      let hs_file = dir </> "Main.hs"
      let obj_file = dir </> "submit.o"
      let exe_file = dir </> "Main"
      T.writeFile c_file c_code
      T.writeFile hs_file props
      gcc:cc_args <- parseArgs gcc_cmd
      ghc:hc_args <- parseArgs ghc_cmd
      let cc_args'= cc_args ++ ["-c", c_file, "-o",  obj_file]
      let hc_args'= hc_args ++ ["-i"++dir, obj_file, hs_file, "-o",  exe_file]
      chmod executable dir
      chmod writeable dir
      chmod readable c_file
      -- compile C code to object file
      runProcess (Just limits) gcc cc_args'
      -- compile Haskell quickcheck driver
      runProcess Nothing ghc hc_args'
      -- allow anyone to execute the binary (for safeExec)
      chmod readable exe_file
      -- execute and under safeExec and classify result
      classify <$> safeExec limits exe_file Nothing qcArgs ""


classify :: (ExitCode, Text, Text) -> Result
classify (ExitSuccess, stdout, _)  = accepted (P.codeBlock stdout)
classify (ExitFailure _, stdout, stderr)
  | match "Time Limit" stderr   = timeLimitExceeded (P.codeBlock msg)
  | match "Memory Limit" stderr = memoryLimitExceeded (P.codeBlock msg)
  | match "Command terminated by signal" stderr
                                = runtimeError (P.codeBlock msg)
  | match "Failed!" stdout       = wrongAnswer (P.codeBlock stdout) 
  | match "Failures:" stdout     = wrongAnswer (P.codeBlock (stdout<>stderr))
  | match "Exception" stdout     = runtimeError (P.codeBlock stdout)
  | match "Command exited with non-zero status" stderr
                                 = runtimeError (P.codeBlock msg)
  | otherwise                    = miscError (P.codeBlock msg)
  where msg = stdout <> stderr



invalidSubmission :: Result
invalidSubmission
  = miscError $
    P.header 2 "Invalid submission" <>
    P.plain "You submission was rejected because it uses some forbidden language feature."

    
    
