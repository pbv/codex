--------------------------------------------------------------------------
-- Test complete programs using input/output files
--------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RecordWildCards #-}

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
import           Data.List(minimumBy)
import           Data.Ord(comparing)
import           Text.Printf

import           Control.Exception (catch)
import           System.Directory.Glob

--
-- | build and run scripts for testing standalone programs;
-- the type for executables is existentially quantified (hidden);
-- this generality isn't used at the moment
--
data Build =
  forall exec.
  Build { checkLanguage :: Language -> Bool
        , makeExec :: FilePath -> Code -> IO exec
        , runExec  :: exec -> Text -> IO (ExitCode, Text, Text)
        }


-- | builder for C programs
--
clangBuild :: Tester Build
clangBuild = do
  cc_cmd <- configured "language.c.compiler"
  limits <- testLimits "language.c.limits"
  let cc:cc_args = words cc_cmd
  let make tmpdir (Code _ code) = do 
        let c_file = tmpdir </> "submit.c"
        let exe_file = tmpdir </> "submit"
        T.writeFile c_file code
        runCompiler cc (cc_args ++ [c_file, "-o", exe_file])
        chmod readable exe_file
        chmod executable tmpdir
        return exe_file
  let run exe_file stdin = do
        safeExec limits exe_file [] stdin
  return (Build (=="c") make run)


-- | builder for Python programs
--
pythonBuild ::  Tester Build
pythonBuild = do
  python <- configured "language.python.interpreter"
  limits <- testLimits "language.python.limits"
  let make tmpdir (Code _ code)  = do
        let pyfile = tmpdir </> "submit.py"
        T.writeFile pyfile code
        chmod readable pyfile
        chmod executable tmpdir
        return pyfile
  let run pyfile stdin = do
        safeExec limits python [pyfile] stdin
  return (Build (=="python") make run)

-- | builder for Java programs
--
javaBuild :: Tester Build
javaBuild = do
  javac_cmd <- configured "language.java.compiler"
  java_cmd <- configured "language.java.runtime"
  limits <- testLimits "language.java.limits"
  -- name for public class with the main entry point
  classname <- fromMaybe "Main" <$> metadata "class" 
  let javac:javac_args = words javac_cmd
  let java:java_args = words java_cmd
  let make tmpdir (Code _ code) = do
        let java_file = tmpdir </> classname <.> "java"
        let classfile = tmpdir </> classname <.> "class"
        T.writeFile java_file code
        runCompiler javac (javac_args ++ [java_file])
        chmod readable classfile
        chmod executable tmpdir
        return classfile
  let run classfile stdin = do
        let dir = takeDirectory classfile
        safeExec limits java (java_args ++ ["-cp", dir, classname]) stdin
  return (Build (=="java") make run)


-- builder for Haskell programs
haskellBuild :: Tester Build
haskellBuild = do
  ghc_cmd <- configured "language.haskell.compiler"
  limits <- testLimits "language.haskell.limits"
  -- name for the main module
  modname <- fromMaybe "Main" <$> metadata "module" 
  let ghc:ghc_args = words ghc_cmd
  let make tmpdir (Code _ code) = do
        let hs_file = tmpdir </> modname <.> "hs"
        let exe_file = tmpdir </> modname
        T.writeFile hs_file code
        runCompiler ghc (ghc_args ++ [hs_file, "-o", exe_file])
        chmod readable exe_file
        chmod executable tmpdir
        return exe_file
  let run exe_file stdin = do
        safeExec limits exe_file [] stdin
  return (Build (=="haskell") make run)
        


-- | parametrized I/O tester 
--
stdioTester ::  Build -> Tester Result
stdioTester Build{..} = tester "stdio" $ do
  code@(Code lang _) <- testCode
  guard (checkLanguage lang)
  ---
  dir <- takeDirectory <$> testPath
  inpatts <- map (dir</>) . fromMaybe [] <$> metadata "inputs"
  outpatts <- map (dir</>) . fromMaybe [] <$> metadata "outputs"
  assert (pure $ not (null inpatts)) "no inputs defined"
  assert (pure $ not (null outpatts)) "no outputs defined"
  inputs <- liftIO $ globMany globDefaults inpatts
  outputs <- liftIO $ globMany globDefaults outpatts
  assert (pure $ length inputs == length outputs)
    "different number of inputs and outputs"
  limit <- fromMaybe maxBound <$> metadata "visible" 
  liftIO $ (withTempDir "codex" $ \tmpdir -> do
    exe_file <- makeExec tmpdir code
    aggregate limit <$> runMany exe_file inputs outputs) `catch` return
  where
    runMany exe_file = zipWithM (runSingle exe_file) 
    runSingle exe_file in_file out_file = do
      in_txt <- T.readFile in_file
      out_txt <- T.readFile out_file
      classify in_txt out_txt <$> runExec exe_file in_txt


-- | aggregate all tests results into a single one
aggregate :: Int -> [Result] -> Result
aggregate limit = worstResult . numberResults . hideDetails limit

-- | combine results into a single most severe result
worstResult :: [Result] -> Result
worstResult results
  = minimumBy (comparing resultClassify) (summary:results)
  where
    total = length results
    summary = accepted $ "Passed " <> T.pack (show total) <> " tests"


classify :: Text -> Text -> (ExitCode, Text, Text) -> Result
classify _ _ (ExitFailure c, _, err) 
  = runtimeError $ T.unlines
    [ "Program exited with non-zero status: " <> T.pack (show c)
    ,  err
    ]
classify input expected (_, out, err)
  | match "Command terminated by signal" err ||
    match "Command exited with non-zero status" err
  = runtimeError $
    T.unlines [ "Input:", input
              , "Expected output:", expected
              , "Obtained ouput:", out, err
              ]
classify input expected (_, out, err)
  | match "Time Limit" err =
    timeLimitExceeded $
    T.unlines [ "Input:", input
              , "Expected output:", expected
              , "Obtained output:", out, err
              ]
classify input expected (_, out, err)
  | match "Memory Limit" err =
    memoryLimitExceeded $
    T.unlines [ "Input:", input
              , "Expected output:", expected
              , "Obtained output:", out, err
              ]
classify input expected (_, out, err)
  | match "Output Limit" err =
    runtimeError $ 
    T.unlines [ "Input:", input
              , "Expected output:", expected
              , "Obtained output:", out, err
              ]
classify _ expected (_, out, _) 
  | out == expected
  = accepted "Passed" 
classify input expected (_, out, _) 
  = (if T.strip out == T.strip expected
      then
        presentationError
      else wrongAnswer) $
    T.unlines
    [ "Input:", input
    , "Expected output:", expected
    , "Obtained output:", out
    ]
  

-- number each test report
numberResults :: [Result] -> [Result]
numberResults results = zipWith number [1..total] results
  where
    total = length results
    test num = T.pack $ printf "*** Test %d / %d ***\n" num total
    number n Result{..}
      = Result resultClassify (test n <> resultMessage)


-- hide feedback detail for after some number of tests
hideDetails :: Int -> [Result] -> [Result]
hideDetails n results
  = take n results ++ map (\r -> r{resultMessage = msg}) (drop n results)
  where
    msg = T.pack $
          printf "*** input / output details hidden after %d tests ***" n
