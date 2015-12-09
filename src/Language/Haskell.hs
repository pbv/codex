{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Language.Haskell where

import           Control.Applicative
import           Data.String
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Data.Monoid
import           Database.SQLite.Simple.ToField
import           Database.SQLite.Simple.FromField

import           System.FilePath
import           System.Directory
import           System.IO

import           Types
import           Tester
import           SafeExec

-- | Haskell configuration
data HaskellConf = HaskellConf { haskellExec :: !FilePath
                               , haskellSfConf :: !SafeExecConf
                               } deriving (Eq, Show)


--------------------------------------------------------------------------
-- test haskell with QuickCheck
--------------------------------------------------------------------------

haskellTesterIO :: HaskellConf
                   -> Tests 
                   -> Code
                   -> IO (Result,Text)
haskellTesterIO HaskellConf{..} (Tests props) (Code code) = 
   withTempFile "Temp.hs" $ \(codefile, h) ->
   let codemod = T.pack (takeBaseName codefile)
       dir = takeDirectory codefile
   in do T.hPutStrLn h (moduleHeader codemod)
         T.hPutStrLn h code
         hClose h
         withTextTemp "Main.hs" (testScript codemod props) $ \tstfile -> 
           safeExecWith haskellSfConf haskellExec ["-i"++dir, tstfile] "" >>=
           (return . haskellResult)



testScript :: Text -> Text -> Text
testScript codemod props
  = T.unlines
    [ languageHeader "TemplateHaskell",
      moduleHeader "Main",
      importHeader "System.Exit",
      importHeader "Test.QuickCheck",
      importHeader "Test.QuickCheck.Function",
      importHeader codemod,
      "",
      props,
      "",
      "return []",
      "main = $quickCheckAll >>= \\c -> if c then exitSuccess else exitFailure"
    ]
    

moduleHeader :: Text -> Text
moduleHeader name = "module " <> name <> " where"

importHeader :: Text -> Text
importHeader mod = "import "<>  mod 

languageHeader :: Text -> Text
languageHeader ext =  "{-# LANGUAGE " <> ext <> "#-}"

{-
  putStr "exitCode=" >> print exitCode
  putStrLn "stdout=" >> putStrLn (T.unpack stdout)
  putStrLn "stderr=" >> putStrLn (T.unpack stderr)  
  return (result,msg)
-}

haskellResult (exitCode, stdout, stderr)  
  | match "Not in scope" stderr ||
    match "parse error" stderr  ||
    match "Couldn't match" stderr  = (CompileError, stderr)
  | match "Time Limit" stderr   = (TimeLimitExceeded, stderr)
  | match "Memory Limit" stderr = (MemoryLimitExceeded, stderr)
  | match "Failed" stdout       = (WrongAnswer, stdout)
  | match "Command exited with non-zero status" stderr = (MiscError, stderr)
  | otherwise = (Accepted, stdout)


