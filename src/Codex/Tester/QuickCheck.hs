{-
  Utility functions for QuickCheck-related stuff
-}
{-# LANGUAGE OverloadedStrings #-}

module Codex.Tester.QuickCheck where

import           Data.Maybe(catMaybes)
import           Codex.Tester
import           System.FilePath

-- relative filepath to Quickcheck properties
getQuickCheckPath :: FilePath -> Meta -> Maybe FilePath
getQuickCheckPath base meta
  = (base </>) <$> lookupFromMeta "quickcheck" meta


-- get QuickCheck runner command line arguments
getQuickCheckArgs :: Meta -> [String]
getQuickCheckArgs meta
  = catMaybes
    [ lookupArg "maxSuccess"
    , lookupArg "maxSize"
    , lookupArg "maxDiscardRatio"
    , lookupArg "randSeed"
    ]
  where
    lookupArg key
      = fmap (\val -> "--" ++ key ++ "=" ++ val) (lookupFromMeta key meta)






