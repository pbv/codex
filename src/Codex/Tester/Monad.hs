{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-
-- a monad for writing testers for submissions
-- allows access to a an environment, ruinning IO and failure (i.e. passing)
--
-}
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
             -- , _testSubmitted :: Code  -- ^ submited language & code
             -- , _testUser :: UserLogin  -- ^ user 
             } 


-- | run function for testers
runTester :: Tester a
          -> Config -> FilePath -> Page -> Submission
          -> IO (Maybe a)
runTester m cfg filepath page sub 
  = let env = TestEnv cfg filepath page sub
    in runMaybeT $ runReaderT (unTester m) env


-- | fetch parameters from environment
askConfig :: Tester Config
askConfig = Tester (asks _testConfig)

askPath :: Tester FilePath
askPath = Tester (asks _testFilePath)

askSubmitted :: Tester Code
askSubmitted = Tester (submitCode <$> asks _testSubmission)

askUser :: Tester UserLogin
askUser = Tester (submitUser <$> asks _testSubmission)

askPage :: Tester Page
askPage = Tester (asks _testPage)

askMetadata :: Tester Meta
askMetadata = pageMeta <$> askPage


-- | get a medata value from a field
metadata :: FromMetaValue a => String -> Tester (Maybe a)
metadata key = do
  meta <- askMetadata
  return (lookupFromMeta key meta)

-- | get an absolute path from metadata field
metadataPath :: String -> Tester (Maybe FilePath)
metadataPath key = do
  dir <- takeDirectory <$> askPath   -- directory for exercise page
  fmap (dir </>) <$> metadata key
                      -- lookup key and make absolute path if found


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
                  




