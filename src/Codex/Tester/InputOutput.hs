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
import           Data.Diff.Myers (diffTexts, Edit(..))
import           Data.Foldable (toList)
import           Data.Maybe (fromMaybe)
import           Data.Char (isSpace)
import           Control.Exception(handle)
import           System.Directory(copyFile)


import qualified Text.Pandoc.Builder as P
import           Text.Pandoc.Walk (walk)

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
  limits <- configLimits "language.c.limits"
  cc:cc_args <- parseArgs cc_cmd
  let make tmpdir (Code _ code) = do
        let c_file = tmpdir </> "submit.c"
        let exe_file = tmpdir </> "submit"
        T.writeFile c_file code
        chmod (executable . readable . writeable) tmpdir
        chmod readable c_file
        runProcess (Just limits) cc (cc_args ++ [c_file, "-o", exe_file])
        return exe_file
  let run exe_file dir args stdin = do
        safeExec limits exe_file dir args stdin
  return (Build (=="c") make run)


-- | builder for Python programs
--
pythonBuild ::  Tester Build
pythonBuild = do
  python <- configured "language.python.interpreter"
  limits <- configLimits "language.python.limits"
  let make tmpdir (Code _ code)  = do
        let pyfile = tmpdir </> "submit.py"
        T.writeFile pyfile code
        chmod (readable . executable) tmpdir
        chmod readable pyfile
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
  limits <- configLimits "language.java.limits"
  -- name for public java class with the main method
  classname <- fromMaybe "Main" <$> metadata "java-main"
  javac:javac_args <- parseArgs javac_cmd
  java:java_args <- parseArgs java_cmd
  let make tmpdir (Code _ code) = do
        let java_file = tmpdir </> classname <.> "java"
        let classfile = tmpdir </> classname <.> "class"
        T.writeFile java_file code
        chmod (executable . readable . writeable) tmpdir
        chmod readable java_file
        runProcess (Just limits) javac (javac_args ++ [java_file])
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
  limits <- configLimits "language.haskell.limits"
  -- name for the Haskell main module
  modname <- fromMaybe "Main" <$> metadata "haskell-main"
  ghc:ghc_args <- parseArgs ghc_cmd
  let make tmpdir (Code _ code) = do
        let hs_file = tmpdir </> modname <.> "hs"
        let exe_file = tmpdir </> modname
        T.writeFile hs_file code
        chmod (readable . writeable . executable) tmpdir
        chmod readable hs_file
        runProcess (Just limits) ghc (ghc_args ++ [hs_file, "-o", exe_file])
        return exe_file
  let run exe_file args stdin = do
        safeExec limits exe_file args stdin
  return (Build (=="haskell") make run)



-- | parameterized I/O tester 
--
stdioTester ::  Build -> Tester Result
stdioTester Build{..} = tester "stdio" $ do
  code@(Code lang _) <- testCode
  guard (checkLanguage lang)
  dir <- takeDirectory <$> testFilePath
  -- input and output files
  inputs <-  globPatterns dir =<< metadataWithDefault "inputs" []             
  outputs <- globPatterns dir =<< metadataWithDefault "outputs" [] 
  let num_inputs = length inputs
  let num_outs = length outputs
  assert (pure $ num_inputs == num_outs)
    "different number of public inputs and outputs"
  arguments <- metadataWithDefault "public-arguments" (replicate num_inputs "")  
  let num_args = length arguments
  assert (pure $ num_args == num_inputs)
    "different number of public arguments and inputs"

  priv_inputs <- globPatterns dir =<< metadataWithDefault "private-inputs" []
  priv_outputs <- globPatterns dir =<< metadataWithDefault "private-outputs" []
  let num_priv_inputs = length priv_inputs
  let num_priv_outputs = length priv_outputs
  assert (pure $ num_priv_inputs == num_priv_outputs) 
     "different number of private inputs and outputs"
  priv_arguments <- metadataWithDefault "private-arguments" (replicate num_priv_inputs "")
  let num_priv_args = length priv_arguments
  assert (pure $ num_priv_args == num_priv_inputs)
     "different number of private arguments and inputs"
  assert (pure $ num_inputs + num_priv_inputs > 0)
    "no test cases defined"
  --- extra files to copy to temp directory
  files <- globPatterns dir =<< metadataWithDefault "files" []
  -- run the test and agregate results
  liftIO $
    withTempDir "codex" $ \tmpdir -> handle compileErrorHandler $ do
        -- make temp directory readable, writable and executable for user 
        chmod (readable . writeable . executable) tmpdir
        -- copy extra files
        mapM_ (\f -> copyFile f (tmpdir </> takeFileName f)) files
        -- make executable
        exe_file <- makeExec tmpdir code
        -- run all tests
        r1 <- runTests (runExec exe_file (Just tmpdir)) $ 
                  zip3 arguments inputs outputs
        r2 <- runTests (runExec exe_file (Just tmpdir)) $ 
                  zip3 priv_arguments priv_inputs priv_outputs
        return (tagWith Public r1 <> tagWith Private r2)


-- run tests until the 1st failure
runTests :: ([String] -> Text -> IO (ExitCode, Text, Text))   -- action 
         -> [(String, FilePath, FilePath)]  -- ^ args, input path, output path
         -> IO Result  
runTests _ [] = mempty
runTests action tests
  = loop 1 tests
  where
    total = length tests
    loop _ []
      = return $ accepted $ P.plain $ P.text $ T.pack (show total) <>  " tests passed."
    loop n ((arg_str, in_file, out_file) : tests) = do
      result <- runTest action arg_str in_file out_file
      if resultStatus result == Accepted then
        loop (n+1) tests
        else
        return result
               

-- run a single test case
runTest :: ([String] -> Text -> IO (ExitCode, Text, Text)) 
        -> String -> FilePath -> FilePath -> IO Result
runTest action arg_str in_file out_file = do
      in_txt <- T.readFile in_file
      out_txt <- T.readFile out_file
      args <- parseArgs arg_str
      classify arg_str in_txt out_txt <$> action args in_txt
    

classify ::  String -> Text -> Text -> (ExitCode, Text, Text) -> Result
classify args input expected (ExitSuccess, out, _)
  | out == expected 
      = accepted (P.plain $ P.str "OK")
  | removeSpaces out == removeSpaces expected 
      = presentationError $ textDiff args expected input out
  | otherwise 
      = wrongAnswer $ textDiff args expected input out
classify args input _ (ExitFailure c, _, err)
  | match "Time Limit" err   
      = timeLimitExceeded $ textInput args input
  | match "Memory Limit" err 
      = memoryLimitExceeded $ textInput args input
  | match "Output Limit" err 
      = runtimeError (textInput args input <> P.codeBlock err)
  | otherwise 
      = runtimeError $ 
                (textInput args input <> 
                P.plain (P.str ("Program exited with non-zero status: " <> T.pack (show c))) <>
                P.codeBlock err)
                
removeSpaces :: Text -> Text
removeSpaces = T.filter (not.isSpace)

textInput :: String -> Text -> P.Blocks
textInput args input  
  = P.simpleTable []   
    [ cmdlineArgs args
    , [P.plain $ P.strong $ P.str "Input", P.codeBlock input]
    ]

textDiff :: String -> Text -> Text -> Text -> P.Blocks
textDiff args expected input out =
    P.simpleTable []
    [ cmdlineArgs args
    , [P.plain $ P.strong $ P.str "Input", P.codeBlock input]
    , [P.plain $ P.strong $ P.str "Expected Output", P.codeBlock expected]
    , [P.plain $ P.strong $ P.str "Obtained Output", P.codeBlock out]
    , [P.plain $ P.strong $ P.str "Differences", 
       P.divWith ("", ["text-diffs"],[]) $ P.plain $ showDiffs out expected]
    ]

cmdlineArgs :: String -> [P.Blocks]
cmdlineArgs args
  = if null args then [] else
    [P.plain $ P.strong $ P.str "Command-line arguments", P.codeBlock (T.pack args)]

-- show text differences as strikeouts/underlines
showDiffs :: Text -> Text -> P.Inlines
showDiffs txt1 txt2 
  = walk harden $ go 0 (toList $ diffTexts txt1 txt2)
  where 
      harden :: P.Inline -> P.Inline   -- make all softbreaks into hardbreaks
      harden P.SoftBreak = P.LineBreak
      harden i = i

      text :: Text -> P.Inlines
      text s = if T.null s then mempty else P.text s
      
      go :: Int -> [Edit] -> P.Inlines
      go i [] = text (T.drop i txt1)  
      go i (EditDelete from to : rest)
        = let before  = trim i (from-1) txt1 
              deleted = trim from to txt1
          in text before <> P.strikeout (text deleted) <> go (to+1) rest 
      go i (EditInsert pos from to : rest) 
         = let before = trim i (pos-1) txt1 
               inserted  = trim from to txt2  
           in text before <> P.underline (text inserted) <> go pos rest     

      trim :: Int -> Int -> Text -> Text
      trim i j txt = T.take (j-i+1) (T.drop i txt)

