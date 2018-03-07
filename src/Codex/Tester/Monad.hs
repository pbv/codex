{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Codex.Tester.Monad (
  Test,
  runTest,
  configured,
  maybeConfigured,
  currentLimits,
  getLimits,
  safeExec,
  safeExecIO,
  -- * modules re-export
  module Control.Monad.Trans
  ) where


import           Data.Text(Text)
import qualified Data.Text as T

import           Data.Configurator.Types
import qualified Data.Configurator as Conf

import           Control.Applicative
import           Control.Monad.Trans
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.State.Strict

import           Codex.Tester.Limits

import           System.Exit(ExitCode)
import           System.Process.Text (readProcessWithExitCode)

-- | a monad for testing scripts
-- allows IO, access to a configuration environment and failure 
newtype Test a
  = Test { unTest :: ReaderT Config (StateT Limits (MaybeT IO)) a }
  deriving (Functor, Monad, Applicative, Alternative, MonadIO)


-- | run a test
--
runTest :: Config -> Limits -> Test a -> IO (Maybe a)
runTest cfg limits action = do
  runMaybeT $ evalStateT (runReaderT (unTest action) cfg) limits

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


getLimits :: Name -> Test Limits
getLimits prefix = do
  cfg <- Test ask
  limits <- liftIO $ configLimits (Conf.subconfig prefix cfg)
  Test (lift $ modify (limits`mappend`))
  currentLimits
                  
currentLimits :: Test Limits
currentLimits = Test (lift get)
  
-- | run a command under SafeExec
safeExec ::  FilePath            -- ^ command
           -> [String]           -- ^ arguments
           -> Text               -- ^ stdin
           -> Test (ExitCode, Text, Text) -- ^ code, stdout, stderr
safeExec cmd args stdin = do
  limits <- currentLimits
  liftIO $ safeExecIO limits cmd args stdin


safeExecIO :: Limits            -- ^ resource limits
           -> FilePath          -- ^ command
           -> [String]          -- ^ arguments
           -> Text              -- ^ stdin
           -> IO (ExitCode, Text, Text)  -- ^ code, stdout, stderr
safeExecIO Limits{..} cmd args stdin
  = let mkArg opt = maybe [] (\c -> [opt, show c])
        args' = mkArg "--cpu" maxCpuTime
                ++
                mkArg "--clock" maxClockTime
                ++
                mkArg "--mem" maxMemory
                ++
                mkArg "--stack" maxStack
                ++
                mkArg "--fsize" maxFSize
                ++
                mkArg "--core" maxCore
                ++
                mkArg "--nproc" numProc
                ++
                ["--exec", cmd] ++
                args
    in
      readProcessWithExitCode "safeexec" args' stdin



