{-# LANGUAGE OverloadedStrings #-}
module Codex.Tester (
  oneOf,
  tester,
  language,
  restrict,
  nullTester,
  -- * module re-exports
  Meta, Code(..),
  lookupFromMeta, formatDiffs, 
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
import           Codex.Page (lookupFromMeta, formatDiffs)
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
import           Text.Regex.TDFA((=~~))


-- | Try testers in order, return the first one that suceedds.
-- This is just `asum` from Control.Applicative.Alternative
-- renamed for readability
oneOf :: [Tester a] -> Tester a
oneOf = foldr (<|>) empty

  
-- | trivial tester (accepts all submissions)
nullTester :: Tester Result
nullTester = tester "accept" $ return $ accepted (P.plain $ P.text "Submission recorded")


-- | check the tester label
tester :: Text -> Tester a -> Tester a
tester name cont = do
  meta <- testMetadata
  guard (lookupFromMeta "tester" meta == Just name)
  cont

-- | check the submission language
language :: Language -> Tester a -> Tester a
language name cont = do
  Code lang _ <- testCode
  guard (lang == name)
  cont

-- | check for restricted expressions in the code
restrict :: Tester Result -> Tester Result
restrict cont = do
  opt <- metadata "forbid" :: Tester (Maybe Text)
  case opt  of
    Nothing -> cont
    Just re -> do
      Code _ code <- testCode
      case (code =~~ re) of
        Nothing -> cont
        Just match -> return $ invalidSubmission match


invalidSubmission :: Text -> Result
invalidSubmission match
  = miscError $
    P.header 2 "Invalid submission" <>
    P.plain "Rejected because of the following:" <>
    P.codeBlock match
          

