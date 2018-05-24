
module Codex.Tester (
  Tester, PageInfo(..),
  language,
  tester,
  oneof,
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

import           Data.Text (Text)

-- | an exercise tester 
type Tester = PageInfo -> Code -> Test Result

data PageInfo
  = PageInfo { infoPath :: FilePath
             , infoMeta :: Meta
             } deriving Show

language :: Meta -> Maybe Language
language = lookupFromMeta "language"

tester :: Meta -> Maybe Text
tester = lookupFromMeta "tester"

-- | combine a list of testers in sequence
oneof :: [Tester] -> Tester
oneof list info code 
  = foldr (\t r -> t info code <|> r) empty list

