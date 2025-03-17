{-# LANGUAGE OverloadedStrings #-}
--------------------------------------------------------------------------
-- Test Haskell and C code using QuickCheck
--------------------------------------------------------------------------
module Codex.Tester.QuickCheck (
  haskellQCTester,
  clangQCTester
  ) where


import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Data.Maybe (fromMaybe)
import           Codex.Tester 
import           System.Directory(copyFile)
import           Control.Exception(handle)

import qualified Text.Pandoc.Builder as P

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
  liftIO (haskellRunner limits ghc qcArgs files src props)


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


clangQCTester :: Tester Result
clangQCTester = tester "quickcheck" $ do
  Code lang src <- testCode
  guard (lang == "c")
  path <- testFilePath
  qcpath <- fromMaybe (replaceExtension path ".hs")
            <$> metadataPath "properties"
  assert (fileExists qcpath)
    ("properties file not found: " <> show qcpath)
  props  <- liftIO (T.readFile qcpath)
  ghc    <- configured "language.haskell.compiler"
  gcc    <- configured "language.c.compiler"
  limits <- configLimits "language.haskell.limits"
  qcArgs <- getQuickCheckArgs <$> testMetadata
  -- append an optional header (for includes, prototypes, etc.)
  header <- fromMaybe "" <$> metadata "header"
  let code = header <> "\n" <> src
  liftIO (clangRunner limits gcc ghc qcArgs code props)

clangRunner :: Limits -> String -> String -> [String] -> Text -> Text
            -> IO Result
clangRunner limits gcc_cmd ghc_cmd qcArgs c_code props =
  withTempDir "codex" $ \dir -> handle compileErrorHandler $ do
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
  | match "Command exited with non-zero status" stderr
                                = runtimeError (P.codeBlock msg)
  | otherwise                  = miscError (P.codeBlock msg)
  where msg = stdout <> stderr


getQuickCheckArgs :: Meta -> [String]
getQuickCheckArgs 
  =  map T.unpack .
     getMetaArgs ["maxSucess", "maxSize", "maxDiscardRatio", "randSeed"] []
