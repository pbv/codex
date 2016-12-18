{-
  Utility functions for QuickCheck-related stuff
-}
{-# LANGUAGE OverloadedStrings #-}

module Language.QuickCheck where

import           Page
import           Markdown

import           Test.QuickCheck
import           Test.QuickCheck.Random
import           System.FilePath



-- relative filepath to Quickcheck properties
getQuickcheckPath :: Page -> Maybe FilePath
getQuickcheckPath p
  = (takeDirectory (pagePath p) </>) <$> lookupFromMeta "quickcheck" (pageMeta p)


getQuickcheckArgs :: Page -> Args
getQuickcheckArgs p = arg1 $ arg2 $ arg3 $ arg4 stdArgs
    where
      meta = pageMeta p
      arg1 = maybe id (\s r->r{maxSuccess=s}) (lookupFromMeta "maxSuccess" meta)
      arg2 = maybe id (\s r->r{maxSize=s}) (lookupFromMeta "maxSize" meta)
      arg3 = maybe id (\s r->r{maxDiscardRatio=s}) (lookupFromMeta "maxDiscardRatio" meta)
      arg4 r = r {replay = fmap (\s -> (mkQCGen s,0)) (lookupFromMeta "randSeed" meta)}






