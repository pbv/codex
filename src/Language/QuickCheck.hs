{-
  Utility functions for QuickCheck-related stuff
-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.QuickCheck where

import           Page
import           Markdown

import           System.FilePath
import           Data.Maybe

-- QuickCheck arguments
data QuickCheckArgs =
  QuickCheckArgs { maxSuccess :: Int
                 , maxDiscardRatio :: Int
                 , maxSize :: Int
                 , optSeed :: Maybe Int
                 } deriving Show


-- get the filepath to Quickcheck properties
-- NB: properties-only (not a module)
getQuickcheckPath :: Page -> FilePath
getQuickcheckPath Page{..} 
  = root </> (maybe 
              (replaceExtension path ".hs")
              (takeDirectory path </>)
              (lookupFromMeta "quickcheck" meta))



getQuickcheckArgs :: Page -> QuickCheckArgs
getQuickcheckArgs Page{..} =
  let success = fromMaybe 100 (lookupFromMeta "maxSuccess" meta)
      size = fromMaybe 100 (lookupFromMeta "maxSize" meta)
      discard = fromMaybe 10 (lookupFromMeta "maxDiscardRatio" meta)
      optSeed = lookupFromMeta "randomSeed" meta
  in QuickCheckArgs { maxSuccess = success,
                      maxSize = size,
                      maxDiscardRatio = discard,
                      optSeed = optSeed
                    }

-- setup string for running using suplied arguments
setupArgs :: QuickCheckArgs -> String
setupArgs QuickCheckArgs{..} =
  "stdArgs { maxSuccess = " ++ show maxSuccess ++
  ", maxSize = " ++ show maxSize ++
  ", maxDiscardRatio = " ++ show maxDiscardRatio ++
  maybe "" (\seed -> ", replay = Just (mkQCGen " ++ show seed ++ ",0)")
  optSeed ++
  " }"
