{-# LANGUAGE OverloadedStrings #-}
module Codex.Tester (
  oneOf,
  tester,
  nullTester,
  -- * module re-exports
  Meta, Code(..),
  lookupFromMeta,
  module Codex.Tester.Monad,
  module Codex.Tester.Result,
  module Codex.Tester.Utils,
  module Codex.Tester.Limits,
  -- * generic stuff
  module Control.Monad,
  module Control.Monad.Trans,
  module System.FilePath,
  module System.Exit,
  module Data.Monoid,
  ) where

import           Codex.Types
import           Codex.Page (lookupFromMeta)
import           Text.Pandoc (Meta)
import qualified Text.Pandoc.Builder as P
import           Codex.Tester.Monad
import           Codex.Tester.Limits
import           Codex.Tester.Result
import           Codex.Tester.Utils
import           Control.Applicative
import           Control.Monad 
import           Control.Monad.Trans

import           System.FilePath
import           System.Exit
import           Data.Monoid


-- | Try testers in order, return the first one that suceedds.
-- This is just `asum` from Control.Applicative.Alternative
-- renamed for readability
oneOf :: [Tester a] -> Tester a
oneOf = foldr (<|>) empty


-- | label a tester and ignore submissions that don't match 
tester :: Text -> Tester a -> Tester a
tester name cont = do
  meta <- testMetadata
  guard (lookupFromMeta "tester" meta == Just name)
  cont
  
-- | trivial tester (accepts all submissions)
nullTester :: Tester Result
nullTester = tester "accept" $ return $ accepted (P.plain $ P.text "Submission recorded")
