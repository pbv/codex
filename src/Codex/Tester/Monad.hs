{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Codex.Tester.Monad (
  Test,
  SafeExec,
  runTest,
  testConfig,
  testPath,
  testPage,
  testSafeExec,
  -- * modules re-export
  module Control.Monad.Trans
  ) where

import           Codex.Types
import           Codex.Page
import           Codex.SafeExec
import           System.Exit (ExitCode)

import           Data.Configurator.Types
import qualified Data.Configurator as Conf

import           Control.Applicative
import           Control.Monad.Trans
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.Reader


-- | a monad for testing;
-- allows IO, access to a configuration environment and failure 
newtype Test a
  = Test { unTest :: ReaderT TestEnv (MaybeT IO) a }
  deriving (Functor, Monad, Applicative, Alternative, MonadIO)

-- | the test environment
--
data TestEnv = TestEnv
               { _testConfig :: !Config   -- ^ handle for config environment
               , _testPath :: !FilePath   -- ^ filepath for the exercise 
               , _testPage :: !Page       -- ^ exercise page (pandoc document)
               } 


-- | run a test
--
runTest :: Config -> FilePath -> Page -> Test a -> IO (Maybe a)
runTest cfg path page test = do
  let env = TestEnv { _testConfig = cfg
                    , _testPath = path
                    , _testPage = page
                    }
  runMaybeT $ runReaderT (unTest test) env



-- | environment access 
testPath :: Test FilePath
testPath = Test (asks _testPath)

testPage :: Test Page
testPage = Test (asks _testPage)

-- | fetch a configuration value
testConfig :: Configured a => Name -> Test a
testConfig name = do
  cfg <- Test (asks _testConfig)
  liftIO $ Conf.require cfg name

testLimits :: Name -> Test Limits
testLimits prefix = do
  cfg <- Test (asks _testConfig)
  liftIO $ getLimits (Conf.subconfig prefix cfg)


-- | a safe exec IO action
-- command, arguments, stdin -> (exitcode, stdout, stderr)
type SafeExec = FilePath -> [String] -> Text -> IO (ExitCode, Text, Text)

-- | fetch safe exec command
-- prefixes for limits are listed most specific to most general
testSafeExec :: [Name] -> Test SafeExec
testSafeExec prefixes = do
  cmd <- testConfig "safeexec" -- safe exec command path
  limits <- mconcat <$> mapM testLimits prefixes
  return (safeExecWith cmd limits)


{-
testerLimits :: Name -> Tester Limits
testerLimits prefix = defaultLimits >>= overrideLimits prefix

defaultLimits :: Tester Limits
defaultLimits = Tester (asks limits)

overrideLimits :: Name -> Limits -> Tester Limits
overrideLimits prefix limits = do
  cfg <- testerConfig
  limits' <- liftIO $ getLimits (Conf.subconfig prefix cfg)
  return (mappend limits' limits)

-- | run continuation only if language matches; argument is submission code
withLanguage :: Language -> (Text -> Tester a) -> Tester a
withLanguage lang k = do
  c <- testerCode
  if codeLang c == lang then k (codeText c) else empty
-}
  
