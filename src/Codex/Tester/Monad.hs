{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Codex.Tester.Monad (
  Tester,
  runTester,
  configured,
  testerPath,
  testerPage,
  testerCode, 
  testerLimits,
  testerSafeExecPath,
  withLanguage,
  -- * modules re-export
  module Control.Monad.Trans,
  module Codex.SafeExec
  ) where

import           Codex.Types
import           Codex.Page
import           Codex.SafeExec

import           Data.Configurator.Types
import qualified Data.Configurator as Conf

import           Control.Applicative
import           Control.Monad.Trans
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.Reader


-- | monad for problem testers;
-- allows IO, optional passing and access to an environment
newtype Tester a
  = Tester {unTester :: ReaderT Env (MaybeT IO) a}
  deriving (Functor, Monad, Applicative, Alternative, MonadIO)

data Env = Env { config :: !Config       -- ^ handle for config enviroment
               , filePath :: !FilePath   -- ^ filepath for the exercise 
               , page :: !Page           -- ^ exercise page (pandoc)
               , code :: !Code           -- ^ submitted code
               , limits :: !Limits       -- ^ safe exec resource limits
               , safeExecPath :: !FilePath
               } 


-- * run a code tester 
runTester :: Config -> FilePath -> Page -> Code -> Tester a -> IO (Maybe a)
runTester cfg filepath page code tst = do
  limits <- getLimits (Conf.subconfig "limits" cfg)
  safeExec <- Conf.require cfg "safeexec"
  let env = Env cfg filepath page code limits safeExec
  runMaybeT $ runReaderT (unTester tst) env


-- * environment access 
testerPath :: Tester FilePath
testerPath = Tester (asks filePath)

testerPage :: Tester Page
testerPage = Tester (asks page)

testerCode :: Tester Code
testerCode = Tester (asks code)

testerConfig :: Tester Config
testerConfig = Tester (asks config)

testerSafeExecPath :: Tester FilePath
testerSafeExecPath = Tester (asks safeExecPath)

configured :: Configured a => Name -> Tester a
configured name = do
  cfg <- testerConfig
  liftIO $ Conf.require cfg name

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

  
