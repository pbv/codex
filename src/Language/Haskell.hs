--------------------------------------------------------------------------
-- Test Haskell code using QuickCheck
--------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

module Language.Haskell (
  haskellTester
  ) where

import           Control.Monad.State
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Data.Monoid

import           System.FilePath
import           System.IO

import           Snap.Core(pass)
import           Snap.Snaplet(getSnapletUserConfig)

import           Language.Types
import           Language.QuickCheck
import           Test.QuickCheck (Args)
import           Tester
import           Application
import           Page
import           SafeExec
import           Config

import           System.Process.Text
import           System.Exit
import           Control.Exception


import qualified Data.Configurator as Configurator


-- running and evaluating Haskell submissions

haskellTester :: Page -> Code -> Codex Result
haskellTester page (Code (Language "haskell") code) = do
    conf <- getSnapletUserConfig
    ghc <- liftIO $ Configurator.require conf "language.haskell.compiler"
    sf <- liftIO $ liftM2 (<>)
          (getSafeExecConf "language.haskell.safeexec" conf)
          (getSafeExecConf "safeexec" conf)
    liftIO $ case getQuickcheckPath page of
      Nothing -> return (miscError "no QuickCheck file specified")
      Just qcpath -> do
        let args = getQuickcheckArgs page
        props <- T.readFile (publicPath </> qcpath)
        haskellTesterIO sf ghc args code props `catch` return
haskellTester _ _  = pass             

     

haskellTesterIO :: SafeExecConf -> String -> Args -> Text -> Text -> IO Result
haskellTesterIO sf ghc qcArgs code props =
   withTempFile "Submit.hs" $ \(hs_file, h) ->      
   let codemod = T.pack $ takeBaseName hs_file
       dir = takeDirectory hs_file
   in do
     T.hPutStrLn h (moduleHeader codemod)
     T.hPutStrLn h code
     hClose h
     withTextTemp "Main.hs" (testScript codemod props) $ \tstfile -> do
       let out_file = dir </> takeBaseName tstfile
       let submit_file = dir </> takeBaseName hs_file
       let cmd:args = words ghc
       let args' = args ++ ["-i"++dir, "-O0", "-dynamic", tstfile, "-o", out_file]
       let temps = [out_file, out_file <.> "o", out_file <.> "hi",
                    submit_file <.> "o", submit_file <.> "hi"]
       finally
         (do runCompiler cmd args'
             haskellResult <$> safeExecWith sf out_file [show qcArgs] "")
         (cleanupFiles temps)




runCompiler cmd args = do
  (exitCode, _, stderr) <- readProcessWithExitCode cmd args ""
  case exitCode of
    ExitFailure _ ->
      throw (compileError stderr)
    ExitSuccess ->
      return ()



testScript :: Text -> Text -> Text
testScript codemod props
  = T.unlines
    [ "{-# LANGUAGE TemplateHaskell #-}",
      "module Main where",
      "import System.Exit",
      "import System.Environment (getArgs)",
      "import Test.QuickCheck",
      "import Test.QuickCheck.Function",
      "import Test.QuickCheck.Random",
      "import qualified " <> codemod <> " as Submit",
      "",
      props,
      "",
      "return []",
      "main = do qcArgs<-fmap (read.head) getArgs; $forAllProperties (quickCheckWithResult qcArgs) >>= \\c -> if c then exitSuccess else exitFailure"
    ]


moduleHeader :: Text -> Text
moduleHeader name
  = T.unlines ["{-# LANGUAGE Safe #-}", "module " <> name <> " where"]

haskellResult (exitCode, stdout, stderr)  
  | match "Not in scope" stderr ||
    match "parse error" stderr  ||
    match "Couldn't match" stderr  = compileError stderr
  | match "Time Limit" stderr   = timeLimitExceeded stderr
  | match "Memory Limit" stderr = memoryLimitExceeded stderr
  | match "Failed" stdout       = wrongAnswer stdout
  | match "Command exited with non-zero status" stderr = miscError stderr
  | otherwise = accepted stdout


