--------------------------------------------------------------------------
-- Test Haskell code using QuickCheck
--------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

module Codex.Tester.Haskell (
  haskellTester
  ) where

import           Control.Monad.State
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import           Data.Monoid

import           System.FilePath
import           System.IO
import           System.Process.Text
import           System.Exit
import           Control.Exception

import qualified Data.Configurator as Conf

import           Test.QuickCheck (Args)

import           Codex.SafeExec
import           Codex.Tester
import           Codex.Tester.QuickCheck

-- | running and evaluating Haskell submissions
haskellTester :: Tester Result
haskellTester = language "haskell" $ \code -> do
    conf <- getConfig
    page <- getPage
    base <-  takeDirectory <$> getFilePath
    liftIO $ do
      ghc <- Conf.require conf "language.haskell.compiler"
      sf1 <- getSafeExecConf (Conf.subconfig "safeexec" conf)
      sf2 <- getSafeExecConf (Conf.subconfig "language.haskell.safeexec" conf)
      let sf = sf2 `override` sf1
      case getQuickcheckPath base page of
        Nothing -> return (miscError "no QuickCheck file specified")
        Just qcpath -> do
          let args = getQuickcheckArgs page
          props <- T.readFile qcpath
          haskellRunner sf ghc args code props `catch` return


haskellRunner :: SafeExecConf -> String -> Args -> Text -> Text -> IO Result
haskellRunner sf ghc qcArgs code props =
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
       let args' = args ++ ["-i"++dir, "-O0", "-dynamic", tstfile,
                            "-o", out_file]
       let temps = [out_file, out_file <.> "o", out_file <.> "hi",
                    submit_file <.> "o", submit_file <.> "hi"]
       finally
         (do runCompiler cmd args'
             haskellResult <$> safeExecWith sf out_file [show qcArgs] "")
         (cleanupFiles temps)


runCompiler :: FilePath -> [String] -> IO ()
runCompiler cmd args = do
  (exitCode, _, err) <- readProcessWithExitCode cmd args ""
  case exitCode of
    ExitFailure _ ->
      throw (compileError err)
    ExitSuccess ->
      return ()



testScript :: Text -> Text -> Text
testScript codemod props
  = T.unlines
    [ "{-# LANGUAGE TemplateHaskell #-}",
      "module Main where",
      "import System.Exit",
      "import System.Environment (getArgs)",
      "import Codex.QuickCheck",
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
