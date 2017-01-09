{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Codex.Tester.Monad (
  Tester, runTester,
  getPage, getCode, getConfig,
  language, getConfigured,
  -- * module re-export
  module Control.Monad.Trans
  ) where

import           Codex.Types
import           Codex.Markdown(Page)

import           Data.Configurator.Types
import qualified Data.Configurator as Configurator

import           Data.Text(Text)

import           Control.Applicative
import           Control.Monad.Trans
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.Reader


-- | monadic type for problem testers;
-- allows IO, optional passing and access to an environment
newtype Tester a
  = Tester {unTester ::  ReaderT (Config,Page,Code) (MaybeT IO) a}
  deriving (Functor, Monad, Applicative, Alternative, MonadIO)

-- * run a code tester 
runTester :: Config -> Page -> Code -> Tester a -> IO (Maybe a)
runTester conf page code m
  = runMaybeT $ runReaderT (unTester m) (conf,page,code) 

-- * access environment
getPage :: Tester Page
getPage = Tester $ asks (\(_, page, _) -> page)

getCode :: Tester Code
getCode = Tester $ asks (\(_, _ ,code) -> code)

getConfig :: Tester Config
getConfig = Tester $ asks (\(conf, _, _) -> conf)

getConfigured :: Configured a => Name -> Tester a
getConfigured name = do
  conf <- getConfig
  opt <- liftIO $ Configurator.lookup conf name
  maybe empty return opt


-- * run a tester on code only if language matches
language :: Language -> (Text -> Tester a) -> Tester a
language lang m = do
  code <- getCode
  if codeLang code == lang then m (codeText code) else empty

