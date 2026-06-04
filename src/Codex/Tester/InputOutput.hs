--------------------------------------------------------------------------
-- Test complete programs using input/output files
--------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Codex.Tester.InputOutput (
  Build,
  clangBuild,
  pythonBuild,
  javaBuild,
  haskellBuild,
  stdioTester,
  ) where

import           Codex.Tester
import           Codex.Types (Language)

import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Data.Maybe (fromMaybe)
import           Data.Char (isSpace)
import           Control.Exception(handle)


import qualified Text.Pandoc.Builder as P

--
-- | build and run scripts for testing standalone programs;
-- the type for executables is existentially quantified (hidden);
-- this generality isn't used at the moment
--
data Build =
  forall exec.
  Build { language :: Language
        , makeExec :: FilePath -> Code -> IO exec
        , runExec  :: exec
                   -> Text           -- stdin
                   -> IO ProcessRun  -- exit code, stdout, stderr
        }


-- | builder for C programs
--
clangBuild :: Tester Build
clangBuild = do
  cc_cmd <- configured "language.c.compiler"
  profile <- configured "language.c.firejail"
  cc:cc_args <- parseArgs cc_cmd
  let make tmpdir (Code _ code) = do
        let cFile = tmpdir </> "submit.c"
        let outFile = tmpdir </> "a.out"
        T.writeFile cFile code
        runProcess profile cc (cc_args ++ [cFile, "-o", outFile])
        return outFile
  let run outFile stdin = do
        let dir = takeDirectory outFile
        sandboxExec profile outFile (Just dir) [] stdin
  return (Build "c" make run)


-- | builder for Python programs
--
pythonBuild ::  Tester Build
pythonBuild = do
  python <- configured "language.python.interpreter"
  profile <- configured "language.python.firejail"
  let make tmpdir (Code _ code)  = do
        let pyfile = tmpdir </> "submit.py"
        T.writeFile pyfile code
        return pyfile
  let run pyfile stdin = do
        let dir = takeDirectory pyfile
        sandboxExec profile python (Just dir) [pyfile] stdin
  return (Build "python" make run)

-- | builder for Java programs
--
javaBuild :: Tester Build
javaBuild = do
  javac_cmd <- configured "language.java.compiler"
  java_cmd <- configured "language.java.runtime"
  profile <- configured "language.java.firejail"
  -- name for public java class with the main method
  classname <- fromMaybe "Main" <$> metadata "java-main"
  javac:javac_args <- parseArgs javac_cmd
  java:java_args <- parseArgs java_cmd
  let make tmpdir (Code _ code) = do
        let java_file = tmpdir </> classname <.> "java"
        let classfile = tmpdir </> classname <.> "class"
        T.writeFile java_file code
        runProcess profile javac (javac_args ++ [java_file])
        return classfile
  let run classfile stdin = do
        let dir = takeDirectory classfile
        let args' = java_args ++ ["-cp", dir, classname] 
        sandboxExec profile java (Just dir) args' stdin
  return (Build "java" make run)


-- builder for Haskell programs
haskellBuild :: Tester Build
haskellBuild = do
  ghc_cmd <- configured "language.haskell.compiler"
  profile <- configured "language.haskell.firejail"
  -- name for the Haskell main module
  modname <- fromMaybe "Main" <$> metadata "haskell-main"
  ghc:ghc_args <- parseArgs ghc_cmd
  let make tmpdir (Code _ code) = do
        let hs_file = tmpdir </> modname <.> "hs"
        let exe_file = tmpdir </> modname
        T.writeFile hs_file code
        runProcess profile ghc (ghc_args ++ ["-i"++tmpdir,
                                                   hs_file, "-o", exe_file])
        return exe_file
  let run outFile stdin = do
        let dir = takeDirectory dir
        sandboxExec profile outFile (Just dir) [] stdin
  return (Build "haskell" make run)



-- | parameterized I/O tester 
--
stdioTester ::  Build -> Tester Result
stdioTester Build{..} = tester "stdio" $ do
  code@(Code lang _) <- testCode
  guard (lang == language)
  dir <- takeDirectory <$> testFilePath
  -- input and output files
  pub_ins <-  globPatterns dir =<< metadataWithDefault "public-inputs" []
  pub_outs <- globPatterns dir =<< metadataWithDefault "public-outputs" [] 
  let num_ins = length pub_ins
  let num_outs = length pub_outs
  assert (pure $ num_ins == num_outs)
    "different number of public inputs and outputs"

  priv_ins <- globPatterns dir =<< metadataWithDefault "private-inputs" []
  priv_outs <- globPatterns dir =<< metadataWithDefault "private-outputs" []
  let num_priv_ins = length priv_ins
  let num_priv_outs = length priv_outs
  assert (pure $ num_priv_ins == num_priv_outs)
    "different number of private inputs and outputs"
  assert (pure $ num_ins + num_priv_ins > 0)
    "no test cases defined"
  --- extra files to copy to temp directory
  files <- globPatterns dir =<< metadataWithDefault "extra-files" []
  -- run the test and agregate results
  withTempDir "codex" $ \tmpdir -> handle compileErrorHandler $ do
        copyFiles files tmpdir         -- copy extra files
        -- make executable
        exeFile <- makeExec tmpdir code
        -- run public and private tests
        (s1, r1) <- runTests (runExec exeFile) $ zip pub_ins pub_outs
        (s2, r2) <- runTests (runExec exeFile) $ zip priv_ins priv_outs
        return (tagWith Public s1 <> tagWith Private s2 <>
                 tagWith Public r1 <> tagWith Private r2)


runTests :: (Text -> IO ProcessRun)   -- action 
         -> [(FilePath, FilePath)]  -- ^ input and output file paths
         -> IO (Result, Result)
runTests _      [] = return (mempty, mempty)
runTests action tests
  = do rs <- mapM (runTest action) tests
       let total  = length tests
       let passed = length (filter ((==Accepted).resultStatus) rs)
       let summary = P.bulletList 
                     [ P.plain ("Test " <> 
                                P.code (T.pack $ takeFileName i) <>
                                P.space <>
                                testResult r)
                     | ((i,_), r) <- zip tests rs
                     ]
                     <>
                     P.plain (P.str (T.pack $ show total) <>
                              " tests, " <>
                              P.str (T.pack $ show passed) <>
                              " passed."
                             )
       return (accepted summary, mconcat rs)
       

-- run a single test case
runTest :: (Text -> IO ProcessRun) -> (FilePath, FilePath)
        -> IO Result
runTest action (in_file, out_file) = do
      in_txt <- T.readFile in_file
      out_txt <- T.readFile out_file
      r <- classify in_txt out_txt <$> action in_txt
      return $
        case resultStatus r of
        Accepted -> mempty  -- nothing to see here
        _ ->  let msg = P.header 3
                        ("FAILED:  " <>
                          P.code (T.pack $ takeFileName in_file))
              in accepted msg <> r 
    

classify :: Text -> Text -> ProcessRun -> Result
classify input expected (ProcessRun ExitSuccess out _)
  | out == expected 
      = mempty
  | removeSpaces out == removeSpaces expected 
      = presentationError (textDiff expected input out)
  | otherwise 
      = wrongAnswer $ textDiff expected input out
classify input _ (ProcessRun (ExitFailure _) _ err)
  | match "TimeLimitExceeded" err
      = timeLimitExceeded (textInput input err) 
  | match "Memory Limit" err 
      = memoryLimitExceeded (textInput input err)
  | match "Output Limit" err 
      = runtimeError (textInput input err)
  | otherwise
      = runtimeError (textInput input err)
                
removeSpaces :: Text -> Text
removeSpaces = T.filter (not.isSpace)

textInput :: Text -> Text -> P.Blocks
textInput input err
  = P.header 3 "Input" <> P.codeBlock input <>
    P.header 3 "Runtime error" <> P.codeBlock err


textDiff :: Text -> Text -> Text -> P.Blocks
textDiff expected input obtained =
  P.header 3 "Input" <>
  P.codeBlock input <>
  P.header 3 "Expected" <>
  P.codeBlock expected <>
  P.header 3 "Obtained" <>
  P.codeBlock obtained <>
  P.header 3 "Differences" <>
  P.divWith ("", ["text-diffs"],[])
      (P.plain $ formatDiffs obtained expected)


testResult :: Result -> P.Inlines
testResult r
  = case resultStatus r of
      Accepted -> P.str "PASS"
      _ -> P.str "FAIL"
