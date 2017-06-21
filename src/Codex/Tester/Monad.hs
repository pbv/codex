{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Codex.Tester.Monad (
  Tester,
  runTester,
  testerPath,
  testerPage,
  testerCode, 
  testerConfig,
  testerSafeExec,
  withLanguage,
  -- * module re-export
  module Control.Monad.Trans,
  module Codex.SafeExec
  ) where

import           Codex.Types
import           Codex.Page
import           Codex.SafeExec

import           Data.Configurator.Types
import qualified Data.Configurator as Configurator

import           Data.Monoid
import           Control.Applicative
import           Control.Monad (liftM2)
import           Control.Monad.Trans
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.Reader


-- | monadic type for problem testers;
-- allows IO, optional passing and access to an environment
newtype Tester a
  = Tester {unTester :: ReaderT Env (MaybeT IO) a}
  deriving (Functor, Monad, Applicative, Alternative, MonadIO)

data Env = Env { config :: !Config       -- ^ handle for config enviroment
               , filepath :: !FilePath   -- ^ filepath for the exercise 
               , page :: !Page           -- ^ exercise page (pandoc)
               , code :: !Code           -- ^ submitted code
               } 


-- * run a code tester 
runTester :: Config -> FilePath -> Page -> Code -> Tester a -> IO (Maybe a)
runTester config filepath page code m
  = runMaybeT $ runReaderT (unTester m) (Env config filepath page code)

-- * environment access 
testerPath :: Tester FilePath
testerPath = Tester (asks filepath)

testerPage :: Tester Page
testerPage = Tester (asks page)

testerCode :: Tester Code
testerCode = Tester (asks code)

testerConfig :: Tester Config
testerConfig = Tester (asks config)


-- | get SafeExec config for a particular language 
testerSafeExec :: Text -> Tester SafeExecConf
testerSafeExec prefix = do
  conf <- testerConfig
  liftIO $ liftM2 mappend 
    (safeExecConfig (Configurator.subconfig (prefix <> ".safeexec") conf))
    (safeExecConfig (Configurator.subconfig "safeexec" conf))


-- | run continuation only if language matches; argument is submission code
withLanguage :: Language -> (Text -> Tester a) -> Tester a
withLanguage lang k = do
  c <- testerCode
  if codeLang c == lang then k (codeText c) else empty

  
