{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Codex.Tester.Monad (
  Tester, runTester,
  getFilePath, getPage, getCode, getConfig,
  language, -- getConfigured,
  -- * module re-export
  module Control.Monad.Trans
  ) where

import           Codex.Types
import           Codex.Page

import           Data.Configurator.Types
import qualified Data.Configurator as Configurator

import           Control.Applicative
import           Control.Monad.Trans
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.Reader


-- | monadic type for problem testers;
-- allows IO, optional passing and access to an environment
newtype Tester a
  = Tester {unTester :: ReaderT Env (MaybeT IO) a}
  deriving (Functor, Monad, Applicative, Alternative, MonadIO)

data Env = Env { config :: Config       -- ^ handle for configurator
               , filepath :: FilePath   -- ^ filepath for the exercise 
               , page :: Page           -- ^ exercise page (pandoc)
               , code :: Code           -- ^ submitted code
               } 


-- * run a code tester 
runTester :: Config -> FilePath -> Page -> Code -> Tester a -> IO (Maybe a)
runTester config filepath page code m
  = runMaybeT $ runReaderT (unTester m) (Env config filepath page code)

-- * environment access 
getFilePath :: Tester FilePath
getFilePath = Tester (asks filepath)

getPage :: Tester Page
getPage = Tester (asks page)

getCode :: Tester Code
getCode = Tester (asks code)

getConfig :: Tester Config
getConfig = Tester (asks config)


-- * run a tester on code only if language matches
language :: Language -> (Text -> Tester a) -> Tester a
language lang m = do
  c <- getCode
  if codeLang c == lang then m (codeText c) else empty

