{-# LANGUAGE OverloadedStrings #-}

module Language(
  module Language.Types,
  module Language.Python,
  module Language.Haskell,
  module Language.C,
  codeTester
  ) where

import Data.Monoid

import Page
import Application
import Control.Applicative
import Tester

import Language.Types
import Language.Python
import Language.Haskell
import Language.C


codeTester :: Page -> Code -> Codex Result
codeTester page code
  = pythonTester page code <|>
    haskellTester  page code <|>
    clangTester page code <|>
    return (received errMsg)
  where
    errMsg = "No tester defined for language \"" <>
             fromLanguage (codeLang code) <> "\""
