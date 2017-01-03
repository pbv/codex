{-# LANGUAGE OverloadedStrings #-}

module Language(
  codeTester, module Language.Types
  ) where

import Data.Monoid
import Data.Configurator.Types

import Page
import Tester

import Language.Types  
import Language.Python 
import Language.Haskell
import Language.C 



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

