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
import           Data.List(sort)
import           Text.Printf

import           Control.Exception (catch)
import           System.FilePath.Glob (glob)

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
  inputs <-  liftIO $ (sort . concat) <$> mapM glob inpatts
  outputs <- liftIO $ (sort . concat) <$> mapM glob outpatts
  assert (pure $ length inputs == length outputs)
    "different number of inputs and outputs"
  liftIO $
    (withTempDir "codex" $ \tmpdir -> do
        exe_file <- makeExec tmpdir code
        runTests (runExec exe_file) $ zip inputs outputs) `catch` return

runTests ::
  (Text -> IO (ExitCode, Text, Text)) -> [(FilePath, FilePath)] -> IO Result
runTests action inouts = test 1 inouts
  where
    total = length inouts
    test _ []
      = return $ accepted $ "Passed " <> T.pack (show total) <> " tests"
    test n ((in_file,out_file):files) = do
      in_txt <- T.readFile in_file
      out_txt <- T.readFile out_file
      result <- classify in_txt out_txt <$> action in_txt
      if resultClassify result == Accepted then
        test (n+1) files
        else
        return (numberResult n total result)
          
numberResult :: Int -> Int -> Result -> Result
numberResult num total Result{..}
  = Result resultClassify (test <> resultMessage)
  where test = T.pack $ printf "*** Test %d / %d ***\n" num total


classify :: Text -> Text -> (ExitCode, Text, Text) -> Result
classify input _ (ExitFailure c, _, err) 
  = runtimeError $ T.unlines
    [ textInput input, ""
    ,  "Program exited with non-zero status: " <> T.pack (show c)
    ,  err
    ]
classify input _ (_, _, err)
  | match "Command terminated by signal" err ||
    match "Command exited with non-zero status" err
  = runtimeError $
    T.unlines [textInput input, err ]
classify input _ (_, _, err)
  | match "Time Limit" err =
    timeLimitExceeded $ textInput input 
classify input _ (_, _, err)
  | match "Memory Limit" err =
    memoryLimitExceeded $ textInput input 
classify input _ (_, _, err)
  | match "Output Limit" err =
    runtimeError $
    T.unlines [textInput input, err]
classify input expected (_, out, _) 
  | T.strip out == T.strip expected
  = accepted "OK"
  | otherwise
  = wrongAnswer $ textDiff expected input out

{-
classify input expected (_, out, _) 
  = 
  = (if T.strip out == T.strip expected
      then presentationError
      else wrongAnswer) $ textDiff expected input out
-}

textInput :: Text -> Text
textInput input =  "Input:\n" <> textHighlight input

textDiff :: Text -> Text -> Text -> Text
textDiff expected input out =
    T.unlines
    [ "Input:", textHighlight input
    , "Expected output:", textHighlight expected
    , "Obtained output:", textHighlight out
    ]

textHighlight :: Text -> Text
textHighlight = T.map (\c -> if c == ' ' then '\x2423' else c) 

