{-
  Utility functions for QuickCheck-related stuff
-}
{-# LANGUAGE OverloadedStrings #-}

module Codex.Tester.QuickCheck where

import           Data.Maybe(catMaybes)
import           Codex.Page

import           System.FilePath

-- relative filepath to Quickcheck properties
getQuickCheckPath :: FilePath -> Page -> Maybe FilePath
getQuickCheckPath base page
  = (base </>) <$> lookupFromMeta "quickcheck" (pageMeta page)


-- get QuickCheck runner command line arguments
getQuickCheckArgs :: Page -> [String]
getQuickCheckArgs page
  = catMaybes
    [ lookupArg "maxSuccess"
    , lookupArg "maxSize"
    , lookupArg "maxDiscardRatio"
    , lookupArg "randSeed"
    ]
  where
    lookupArg key
      = fmap (\val -> "--" ++ key ++ "=" ++ val) (lookupFromMeta key (pageMeta page))






