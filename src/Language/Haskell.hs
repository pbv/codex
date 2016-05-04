--------------------------------------------------------------------------
-- Test Haskell code using QuickCheck
--------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Language.Haskell (
  haskellTester
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

import           Snap.Core(pass)

import           Types
import           Language.Types
import           Markdown
import           Tester
import           Application
import           Page
import           SafeExec


-- QuickCheck arguments
data QuickCheckArgs =
  QuickCheckArgs { maxSuccess :: Int
                 , maxDiscardRatio :: Int
                 , maxSize :: Int
                 , optSeed :: Maybe Int
                 } deriving Show



haskellTester :: Page -> Code -> Codex Result
haskellTester page (Code (Language "haskell") code) = do
    hsConf <- gets haskellConf
    let path = getQuickcheckPath page
    let args = getQuickcheckArgs page
    liftIO $ do
      c <- doesFileExist path
      if c then
        T.readFile path >>= haskellTesterIO hsConf args code 
        else return (miscError $ T.pack $
                     "missing QuickCheck file: " ++ path)
haskellTester _ _  = pass             



-- get the filepath to Quickcheck properties
-- NB: properties-only (not a module)
getQuickcheckPath :: Page -> FilePath
getQuickcheckPath Page{..} 
  = root </> (maybe 
              (replaceExtension path ".hs")
              (takeDirectory path </>)
              (lookupFromMeta "quickcheck" meta))



getQuickcheckArgs :: Page -> QuickCheckArgs
getQuickcheckArgs Page{..} =
  let success = fromMaybe 100 (lookupFromMeta "maxSuccess" meta)
      size = fromMaybe 100 (lookupFromMeta "maxSize" meta)
      discard = fromMaybe 10 (lookupFromMeta "maxDiscardRatio" meta)
      optSeed = lookupFromMeta "randomSeed" meta
  in QuickCheckArgs { maxSuccess = success,
                      maxSize = size,
                      maxDiscardRatio = discard,
                      optSeed = optSeed
                    }


     

haskellTesterIO :: HaskellConf -> QuickCheckArgs -> Text -> Text -> IO Result
haskellTesterIO HaskellConf{..} args code props =
   withTempFile "Temp.hs" $ \(codefile, h) ->      
   let codemod = T.pack (takeBaseName codefile)
       dir = takeDirectory codefile
   in do
     T.hPutStrLn h (moduleHeader codemod)
     T.hPutStrLn h code
     hClose h
     withTextTemp "Main.hs" (testScript args codemod props) $ \tstfile -> 
       haskellResult <$>
       safeExecWith haskellSfConf haskellExec ["-i"++dir, tstfile] "" 



testScript :: QuickCheckArgs -> Text -> Text -> Text
testScript args codemod props
  = T.unlines
    [ "{-# LANGUAGE TemplateHaskell #-}",
      "module Main where",
      "import System.Exit",
      "import Test.QuickCheck",
      "import Test.QuickCheck.Function",
      "import " <> codemod,
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
  | match "Command exited with non-zero status" stderr = miscError stderr
  | otherwise = accepted stdout


