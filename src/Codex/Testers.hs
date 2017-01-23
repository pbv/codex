{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Codex.Testers
  ( allTesters
  -- , getTester
  -- , pythonTester
  -- , haskellTester
  -- , clangTester
  )  where

import           Control.Applicative
-- import           Codex.Types
import           Codex.Tester
import           Codex.Tester.Python
import           Codex.Tester.Haskell
import           Codex.Tester.C

allTesters :: Tester Result
allTesters = pythonTester <|> haskellTester <|> clangTester

{-
getTesters :: [Language] -> Maybe (Tester Result)
getTesters langs = do
    testers <- mapM getTester langs
    return (foldr (<|>) empty testers)

getTester :: Language -> Maybe (Tester Result)
getTester "haskell" = Just haskellTester
getTester "python"  = Just pythonTester
getTester "c"       = Just clangTester 
getTester _         = Nothing
-}  
