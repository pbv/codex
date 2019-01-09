{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Codex.Tester.Monad (
  Tester,
  runTester,
  configured,
  maybeConfigured,
  testLimits,
  testConfig,
  testPath,
  testCode,
  testPage,
  testMetadata,
  testUser,
  metadata,
  ) where



import           Data.Configurator.Types
import qualified Data.Configurator as Conf

import           Control.Applicative
import           Control.Monad.Trans
import           Control.Monad.Trans.Maybe 
import           Control.Monad.Trans.Reader

import           Codex.Types (Page, Code, UserLogin)
import           Text.Pandoc (Meta)
import           Codex.Page
import           Codex.Tester.Limits


-- | a monad for testing scripts
-- allows access to a test environment, IO and failure (i.e. passing)
newtype Tester a
  = Tester { unTester :: ReaderT TestEnv (MaybeT IO) a }
  deriving (Functor, Monad, Applicative, Alternative, MonadIO)

-- | testing environment
data TestEnv
   = TestEnv { _testConfig :: Config   -- ^ static configuration file
             , _testPage :: Page       -- ^ exercise page
             , _testPath :: FilePath   -- ^ file path to exercise page
             , _testCode :: Code       -- ^ submited language & code
             , _testUser :: UserLogin  -- ^ user 
             } 


-- | run function for testers
runTester ::
  Config -> Page -> FilePath -> Code -> UserLogin -> Tester a
  -> IO (Maybe a)
runTester cfg page path code user action
  = runMaybeT $ runReaderT (unTester action) (TestEnv cfg page path code user)


-- | fetch parameters from enviroment
testConfig :: Tester Config
testConfig = Tester (asks _testConfig)

testPath :: Tester FilePath
testPath = Tester (asks _testPath)

testCode :: Tester Code
testCode = Tester (asks _testCode)

testPage :: Tester Page
testPage = Tester (asks _testPage)

testMetadata :: Tester Meta
testMetadata = pageMeta <$> testPage

testUser :: Tester UserLogin
testUser = Tester (asks _testUser)


metadata :: FromMetaValue a => String -> Tester (Maybe a)
metadata key = do
  meta <- testMetadata
  return (lookupFromMeta key meta)


-- | fetch a configured value; return Nothing if key not present
maybeConfigured :: Configured a => Name -> Tester (Maybe a)
maybeConfigured key = do
  cfg <- testConfig
  liftIO $ Conf.lookup cfg key

-- | fetch a configuration value
-- throws an exception if key is not present
configured :: Configured a => Name -> Tester a
configured key = do
  cfg <- testConfig
  liftIO $ Conf.require cfg key


-- | get configured limits from the tester environment
-- overrides default config with the specific one
testLimits :: Name -> Tester Limits
testLimits key = do
  cfg <- testConfig
  liftIO $ do
    def  <- configLimits (Conf.subconfig "limits" cfg)
    spec <- configLimits (Conf.subconfig key cfg)
    return (spec <> def)
                  




