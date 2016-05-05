--------------------------------------------------------------------------
-- Test C code using Haskell QuickCheck
--------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Language.C (
  clangTester
  ) where

import           Control.Applicative
import           Control.Monad.State
import           Data.String
import           Data.Maybe
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Data.Monoid

import           System.FilePath
import           System.Directory
import           System.IO
import           System.Process.Text (readProcessWithExitCode)
import           System.Exit

import           Snap.Core(pass)

import           Types
import           Language.Types
import           Language.QuickCheck
import           Markdown
import           Tester
import           Application
import           Page
import           SafeExec



clangTester :: Page -> Code -> Codex Result
clangTester page (Code (Language "c_cpp") code) = do
  hsConf <- gets haskellConf
  let path = getQuickcheckPath page
  let args = getQuickcheckArgs page
  liftIO $ do
    c <- doesFileExist path
    if c then
      T.readFile path >>= clangTesterIO hsConf args code
      else return (miscError $ T.pack $
                   "missing QuickCheck file: " ++ path)
clangTester _ _ = pass



clangTesterIO HaskellConf{..} args c_code props =
  withTempFile "foreign.c" $ \(c_file, h1) ->
  withTempFile "Test.hs" $ \(hs_file, h2) ->
  let dir = takeDirectory c_file
      obj_file = dir </> takeBaseName c_file <.> "o"
  in do
    -- create C code file
    T.hPutStrLn h1 c_code
    hClose h1
    -- create test script
    T.hPutStrLn h2 (testScript args props)
    hClose h2
    -- compile C code; should this be done under safeExec?
    (exitCode, _, stderr) <- readProcessWithExitCode gccPath ["-fPIC", "-c", c_file, "-o", obj_file] ""
    case exitCode of
      ExitFailure _ -> return (compileError stderr)
      ExitSuccess -> do
        --- run quickCheck
        haskellResult <$> 
            safeExecWith haskellSfConf ghcPath ["-ignore-dot-ghci", "-i"++dir, obj_file, hs_file, "-e", "main"] ""
      -- TODO: cleanup the object file!


ghcPath = "/opt/ghc/7.8.4/bin/ghc"
gccPath = "/usr/bin/gcc"



testScript :: QuickCheckArgs -> Text -> Text
testScript args props
  = T.unlines
    [ "{-# LANGUAGE TemplateHaskell #-}",
      "module Main where",
      "import System.Exit",
      "import Test.QuickCheck",
      "import Test.QuickCheck.Function",
      "",
      props,
      "",
      "return []",
      "main = $forAllProperties (quickCheckWithResult " <>
      "stdArgs { maxSuccess = " <> T.pack (show $ maxSuccess args) <>
      ", maxSize = " <> T.pack (show $ maxSize args) <>
      ", maxDiscardRatio = " <> T.pack (show $ maxDiscardRatio args) <>
      "}) >>= \\c -> if c then exitSuccess else exitFailure"
    ]


moduleHeader :: Text -> Text
moduleHeader name = "module " <> name <> " where"

haskellResult (exitCode, stdout, stderr)  
  | match "Not in scope" stderr ||
    match "parse error" stderr  ||
    match "Couldn't match" stderr  = compileError stderr
  | match "Time Limit" stderr   = timeLimitExceeded stderr
  | match "Memory Limit" stderr = memoryLimitExceeded stderr
  | match "Failed" stdout       = wrongAnswer stdout
  | match "Command terminated by signal" stderr  = runtimeError stderr
  | match "Command exited with non-zero status" stderr = runtimeError stderr
  | otherwise     = accepted stdout

    
