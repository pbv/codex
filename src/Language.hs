{-# LANGUAGE OverloadedStrings #-}

module Language(
  codeTester, module Lang
  ) where

import Control.Applicative
import Data.Monoid

import Page
import Application
import Tester

import Language.Types as Lang
import Language.Python as Lang
import Language.Haskell as Lang
import Language.C as Lang


codeTester :: Page -> Code -> Codex Result
codeTester page code
  = pythonTester page code <|>
    haskellTester  page code <|>
    clangTester page code <|>
    return (received errMsg)
  where
    errMsg = "No tester defined for language \"" <>
             fromLanguage (codeLang code) <> "\""
