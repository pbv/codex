{-# LANGUAGE OverloadedStrings #-}
module Language.Python(
  pythonTester
  ) where

import           Control.Monad (liftM2)
import           Control.Applicative
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as T

import           Language.Types
import           Markdown
import           Page
import           Tester
import           Config
import           SafeExec

import           System.Exit
import           System.FilePath
import           System.Directory

import           Data.Configurator.Types
import qualified Data.Configurator as Configurator


-- | run and evaluate python submissions
pythonTester :: Config -> Page -> Code -> Tester Result
pythonTester conf page (Code (Language "python") code) = tester $ do
    python <- Configurator.require conf "language.python.interpreter"
    sf <- liftM2 (<>)
          (getSafeExecConf "language.python.safeexec" conf)
          (getSafeExecConf "safeexec" conf)
    let tstfile = publicPath </> getDoctest page
    c <- doesFileExist tstfile
    if c then withTextTemp "tmp.py" code $ \pyfile ->
                (Just . pythonResult) <$>
                safeExecWith sf python ["python/pytest.py", tstfile, pyfile] ""
      else return (Just $ miscError $
                   T.pack $ "missing doctest file: " ++ tstfile)
pythonTester _ _ _ = empty


pythonResult :: (ExitCode, Text, Text) -> Result
pythonResult (_, stdout, stderr)
  | T.null stdout && match "OK" stderr = accepted stderr
  | match "Time Limit" stderr          = timeLimitExceeded stderr
  | match "Memory Limit" stderr        = memoryLimitExceeded stderr
  | match "Exception Raised" stdout    = runtimeError stdout
  | match "SyntaxError" stderr         = compileError stderr
  | match "Failed" stdout              = wrongAnswer stdout
  | otherwise                   = miscError (stdout `T.append` stderr)



-- | guess the relative doctest path from metada or the filename
getDoctest :: Page -> FilePath
getDoctest p
  = let path = pagePath p
        meta = pageMeta p
    in maybe
       (replaceExtension path ".tst")
       (takeDirectory path </>)
       (lookupFromMeta "doctest" meta)
