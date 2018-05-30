{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Codex.Tester.Monad (
  Test,
  runTest,
  configured,
  maybeConfigured,
  getLimits,
  ) where



import           Data.Configurator.Types
import qualified Data.Configurator as Conf

import           Control.Applicative
import           Control.Monad.Trans
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.Reader

import           Codex.Tester.Limits


-- | a monad for testing scripts
-- allows IO, access to a configuration environment and failure 
newtype Test a
  = Test { unTest :: ReaderT Config (MaybeT IO) a }
  deriving (Functor, Monad, Applicative, Alternative, MonadIO)


-- | run a test
--
runTest :: Config -> Test a -> IO (Maybe a)
runTest cfg action
  = runMaybeT $ runReaderT (unTest action) cfg

-- | fetch a configuration value
maybeConfigured :: Configured a => Name -> Test (Maybe a)
maybeConfigured name = do
  cfg <- Test ask
  liftIO $ Conf.lookup cfg name 

-- | fetch a configuration value
-- throws an exception if the key is not present
configured :: Configured a => Name -> Test a
configured name = do
  cfg <- Test ask
  liftIO $ Conf.require cfg name 


-- | get configured limits from the test environment
getLimits :: Name -> Test Limits
getLimits prefix = do
  cfg <- Test ask
  liftIO $ lookupLimits (Conf.subconfig prefix cfg)
                  




