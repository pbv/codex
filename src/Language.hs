{-# LANGUAGE OverloadedStrings #-}

module Language(
  module Language.Types,
  module Language.Python,
  module Language.Haskell,
  codeTester
  ) where

import Problem
import Application
import Control.Applicative
import Tester

import Language.Types
import Language.Python
import Language.Haskell


codeTester :: Page -> Code -> AppHandler Result
codeTester page code
  = pythonTester page code <|>
    haskellTester  page code <|>
    return (miscError "no tester defined")
