{-
  Utility functions for QuickCheck-related stuff
-}
{-# LANGUAGE OverloadedStrings #-}

module Codex.Tester.QuickCheck where

import           Data.Maybe(fromMaybe)
import           Codex.Page
import           Codex.QuickCheck.Args

import           System.FilePath

-- relative filepath to Quickcheck properties
getQuickcheckPath :: FilePath -> Page -> Maybe FilePath
getQuickcheckPath base page
  = (base </>) <$> lookupFromMeta "quickcheck" (pageMeta page)


getQuickcheckArgs :: Page -> CodexArgs
getQuickcheckArgs p = arg1 $ arg2 $ arg3 $ arg4 $ arg5 defaultArgs
    where
      meta = pageMeta p
      arg1 = maybe id (\s r->r{maxSuccess=s}) (lookupFromMeta "maxSuccess" meta)
      arg2 = maybe id (\s r->r{maxSize=s}) (lookupFromMeta "maxSize" meta)
      arg3 = maybe id (\s r->r{maxDiscardRatio=s}) (lookupFromMeta "maxDiscardRatio" meta)
      arg4 r = r { randSeed = lookupFromMeta "randSeed" meta }
      arg5 r = r { chatty = fromMaybe True (lookupFromMeta "chatty" meta) }






