{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Codex.Tester.Monad (
  Tester,
  runTester,
  configured,
  maybeConfigured,
  askLimits,
  askConfig,
  askPath,
  askSubmitted,
  askPage,
  askMetadata,
  askUser,
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
             , _testSubmitted :: Code  -- ^ submited language & code
             , _testUser :: UserLogin  -- ^ user 
             } 


-- | run function for testers
runTester ::
  Config -> Page -> FilePath -> Code -> UserLogin -> Tester a
  -> IO (Maybe a)
runTester cfg page path code user action
  = runMaybeT $ runReaderT (unTester action) (TestEnv cfg page path code user)


-- | fetch parameters from enviroment
askConfig :: Tester Config
askConfig = Tester (asks _testConfig)

askPath :: Tester FilePath
askPath = Tester (asks _testPath)

askSubmitted :: Tester Code
askSubmitted = Tester (asks _testSubmitted)

askPage :: Tester Page
askPage = Tester (asks _testPage)

askMetadata :: Tester Meta
askMetadata = pageMeta <$> askPage

askUser :: Tester UserLogin
askUser = Tester (asks _testUser)


metadata :: FromMetaValue a => String -> Tester (Maybe a)
metadata key = do
  meta <- askMetadata
  return (lookupFromMeta key meta)


-- | fetch a configured value; return Nothing if key not present
maybeConfigured :: Configured a => Name -> Tester (Maybe a)
maybeConfigured key = do
  cfg <- askConfig
  liftIO $ Conf.lookup cfg key

-- | fetch a configuration value
-- throws an exception if key is not present
configured :: Configured a => Name -> Tester a
configured key = do
  cfg <- askConfig
  liftIO $ Conf.require cfg key


-- | ask configured limits from the tester environment
-- overrides default config with the specific one
askLimits :: Name -> Tester Limits
askLimits key = do
  cfg <- askConfig
  liftIO $ do
    def  <- configLimits (Conf.subconfig "limits" cfg)
    spec <- configLimits (Conf.subconfig key cfg)
    return (spec <> def)
                  




