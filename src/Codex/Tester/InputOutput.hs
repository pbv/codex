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

import           Data.Char (isSpace, isControl)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Data.Maybe (fromMaybe)
import           Data.List(zip3)
import           Text.Printf

import           Control.Exception (catch)
import           System.Directory(copyFile)

--
-- | build and run scripts for testing standalone programs;
-- the type for executables is existentially quantified (hidden);
-- this generality isn't used at the moment
--
data Build =
  forall exec.
  Build { checkLanguage :: Language -> Bool
        , makeExec :: FilePath -> Code -> IO exec
        , runExec  :: exec
                   -> Maybe FilePath   -- working dir
                   -> [String]         -- cmdline args 
                   -> Text             -- stdin
                   -> IO (ExitCode, Text, Text)  -- status, stdout, stderr
        }


-- | builder for C programs
--
clangBuild :: Tester Build
clangBuild = do
  cc_cmd <- configured "language.c.compiler"
  limits <- askLimits "language.c.limits"
  cc:cc_args <- parseArgs cc_cmd
  let make tmpdir (Code _ code) = do 
        let c_file = tmpdir </> "submit.c"
        let exe_file = tmpdir </> "submit"
        T.writeFile c_file code
        runCompiler cc (cc_args ++ [c_file, "-o", exe_file])
        chmod readable exe_file
        chmod executable tmpdir
        return exe_file
  let run exe_file dir args stdin = do
        safeExec limits exe_file dir args stdin
  return (Build (=="c") make run)


-- | builder for Python programs
--
pythonBuild ::  Tester Build
pythonBuild = do
  python <- configured "language.python.interpreter"
  limits <- askLimits "language.python.limits"
  let make tmpdir (Code _ code)  = do
        let pyfile = tmpdir </> "submit.py"
        T.writeFile pyfile code
        chmod readable pyfile
        chmod executable tmpdir
        return pyfile
  let run pyfile dir args stdin = do
        safeExec limits python dir (pyfile:args) stdin
  return (Build (=="python") make run)

-- | builder for Java programs
--
javaBuild :: Tester Build
javaBuild = do
  javac_cmd <- configured "language.java.compiler"
  java_cmd <- configured "language.java.runtime"
  limits <- askLimits "language.java.limits"
  -- name for public class with the main entry point
  classname <- fromMaybe "Main" <$> metadata "class" 
  javac:javac_args <- parseArgs javac_cmd
  java:java_args <- parseArgs java_cmd
  let make tmpdir (Code _ code) = do
        let java_file = tmpdir </> classname <.> "java"
        let classfile = tmpdir </> classname <.> "class"
        T.writeFile java_file code
        runCompiler javac (javac_args ++ [java_file])
        chmod readable classfile
        chmod executable tmpdir
        return classfile
  let run classfile cwd args stdin = do
        let classpath = takeDirectory classfile
        let args' = java_args ++ ["-cp", classpath, classname] ++ args
        safeExec limits java cwd args' stdin
  return (Build (=="java") make run)


-- builder for Haskell programs
haskellBuild :: Tester Build
haskellBuild = do
  ghc_cmd <- configured "language.haskell.compiler"
  limits <- askLimits "language.haskell.limits"
  -- name for the main module
  modname <- fromMaybe "Main" <$> metadata "module" 
  ghc:ghc_args <- parseArgs ghc_cmd
  let make tmpdir (Code _ code) = do
        let hs_file = tmpdir </> modname <.> "hs"
        let exe_file = tmpdir </> modname
        T.writeFile hs_file code
        runCompiler ghc (ghc_args ++ [hs_file, "-o", exe_file])
        chmod readable exe_file
        chmod executable tmpdir
        return exe_file
  let run exe_file args stdin = do
        safeExec limits exe_file args stdin
  return (Build (=="haskell") make run)
        


-- | parametrized I/O tester 
--
stdioTester ::  Build -> Tester Result
stdioTester Build{..} = tester "stdio" $ do
  code@(Code lang _) <- askSubmitted
  guard (checkLanguage lang)
  ---
  dir <- takeDirectory <$> askPath
  inpatts <- map (dir</>) . fromMaybe [] <$> metadata "inputs"
  outpatts <- map (dir</>) . fromMaybe [] <$> metadata "outputs"
  inputs <-  concat <$> globPatterns inpatts
  outputs <- concat <$> globPatterns outpatts
  let num_inputs = length inputs
  let num_outs   = length outputs
  assert (pure $ num_inputs == num_outs)
    "different number of inputs and outputs"
  assert (pure $ num_inputs /= 0)
    "no test cases defined"
    
  argpatts <- map (dir</>) . fromMaybe [] <$> metadata "arguments"
  argfiles <- concat <$> globPatterns argpatts
  let argfiles' = map Just argfiles ++ repeat Nothing

  filepatts <- map (dir</>) . fromMaybe [] <$> metadata "files"
  files <- concat <$> globPatterns filepatts

  liftIO $
    (withTempDir "codex" $ \tmpdir -> do
        -- make temp directory readable, writable and executable for user 
        chmod (readable . writeable . executable) tmpdir
        -- copy extra files
        mapM_ (\f -> copyFile f (tmpdir </> takeFileName f)) files
        -- make executable
        exe_file <- makeExec tmpdir code
        -- run all tests
        runTests (runExec exe_file (Just tmpdir)) $ zip3 argfiles' inputs outputs)
    `catch` return


runTests ::
  ([String] -> Text -> IO (ExitCode, Text, Text)) ->
  [(Maybe FilePath, FilePath, FilePath)] ->
  -- ^ optional file with cmdline args, input, output
  IO Result
runTests action tests
  = loop 1 tests
  where
    total = length tests
    loop _ []
      = return $ accepted $ "Passed " <> T.pack (show total) <> " tests"
    loop n ((opt_arg_file, in_file, out_file) : tests) = do
      in_txt <- T.readFile in_file
      out_txt <- T.readFile out_file
      arg_str <- maybe (return "") readFile opt_arg_file
      args <- parseArgs arg_str
      result <- classify in_txt out_txt <$> action args in_txt
      if resultStatus result == Accepted then
        loop (n+1) tests
        else
        return (numberResult n total arg_str result)
          
numberResult :: Int -> Int -> String -> Result -> Result
numberResult num total args Result{..}
  = Result resultStatus Valid (test <> resultReport)
  where test
          = T.pack (printf "*** Test %d / %d ***\n\n" num total) <>
          "Command-line arguments:\n" <> T.pack args <> "\n"
          


classify ::  Text -> Text -> (ExitCode, Text, Text) -> Result
classify input _ (ExitFailure c, _, err) 
  = runtimeError $ T.unlines
    [ textInput input, ""
    , "Program exited with non-zero status: " <> T.pack (show c)
    , err
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
textInput input =  "Input:\n" <> sanitize input

textDiff :: Text -> Text -> Text -> Text
textDiff expected input out =
    T.unlines
    [ "Input:", sanitize input
    , "Expected output:", sanitize expected
    , "Obtained output:", sanitize out
    ]

-- | sanitize output text from student's submission;
-- replace spaces with a visible space;
-- replace control chars with the invalid UTF symbol
sanitize :: Text -> Text
sanitize = T.map (\c -> case c of
                     ' ' -> '\x2423'
                     _ -> if isControl c && not (isSpace c)
                          then '\xfffd' else c)

