{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-
-- | Tester monad for writing testers for submissions
-- | allows access to an environment, running IO and failure (i.e. passing)
--
-}
module Codex.Tester.Monad (
  Tester,
  runTester,
  configured,
  maybeConfigured,
  configLimits,
  testConfig,
  testFilePath,
  testCode,
  testPage,
  testMetadata,
  testUser,
  metadata,
  metadataWithDefault,
  metadataPath,
  ) where



import           Data.Configurator.Types
import qualified Data.Configurator as Conf

import           Control.Applicative
import           Control.Monad.Trans
import           Control.Monad.Trans.Maybe 
import           Control.Monad.Trans.Reader

import           Codex.Types (Page, Code, UserLogin)
import           Codex.Submission.Types
import           Text.Pandoc (Meta)
import           Codex.Page
import           Codex.Tester.Limits
import           System.FilePath
import           Data.Maybe (fromMaybe)

-- | a monad for testing scripts
-- allows access to a test environment, IO and failure (i.e. passing)
newtype Tester a
  = Tester { unTester :: ReaderT TestEnv (MaybeT IO) a }
  deriving (Functor, Monad, Applicative, Alternative, MonadIO)

-- | the testing environment
data TestEnv
   = TestEnv { _testConfig :: Config       -- ^ configuration record
             , _testFilePath :: FilePath   -- ^ file path to exercise page
             , _testPage :: Page           -- ^ exercise page
             , _testSubmission :: Submission 
             } 


-- | run function for testers
runTester :: Tester a
          -> Config -> FilePath -> Page -> Submission
          -> IO (Maybe a)
runTester m cfg filepath page sub 
  = let env = TestEnv cfg filepath page sub
    in runMaybeT $ runReaderT (unTester m) env


-- | fetch parameters from environment
testConfig :: Tester Config
testConfig = Tester (asks _testConfig)

testFilePath :: Tester FilePath
testFilePath = Tester (asks _testFilePath)

testCode :: Tester Code
testCode = Tester (submitCode <$> asks _testSubmission)

testUser :: Tester UserLogin
testUser = Tester (submitUser <$> asks _testSubmission)

testPage :: Tester Page
testPage = Tester (asks _testPage)

testMetadata :: Tester Meta
testMetadata = pageMeta <$> testPage


-- | get a medata value given the key
metadata :: FromMetaValue a => String -> Tester (Maybe a)
metadata key = do
  meta <- testMetadata
  return (lookupFromMeta key meta)

-- | get an relative path from metadata 
metadataPath :: String -> Tester (Maybe FilePath)
metadataPath key = do
  dir <- takeDirectory <$> testFilePath  -- directory for exercise page
  fmap (dir </>) <$> metadata key     -- lookup and make a relative path


-- | get a optional value given the default
metadataWithDefault :: FromMetaValue a => String -> a -> Tester a
metadataWithDefault key def = fromMaybe def <$> metadata key

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


-- | ask configured limits from the tester environment
-- overrides default config with the specific one
configLimits :: Name -> Tester Limits
configLimits key = do
  cfg <- testConfig
  liftIO $ do
    def  <- lookupLimits (Conf.subconfig "limits" cfg)
    spec <- lookupLimits (Conf.subconfig key cfg)
    return (spec <> def)
                  




