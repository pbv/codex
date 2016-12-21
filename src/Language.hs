{-# LANGUAGE OverloadedStrings #-}

module Language(
  codeTester, module Lang
  ) where

import Control.Applicative
import Data.Monoid
import Data.Configurator.Types

import Page
import Tester

import Language.Types as Lang
import Language.Python as Lang
import Language.Haskell as Lang
import Language.C as Lang


codeTester :: Config -> Page -> Code -> IO Result
codeTester conf page code
  = case codeLang code of
    Language "python" ->
        pythonTester conf page code
    Language "haskell" ->
        haskellTester conf page code
    Language "c" ->
        clangTester conf page code
    _ -> return (received errMsg)
  where
    errMsg = "No tester defined for language \"" <>
             fromLanguage (codeLang code) <> "\""
