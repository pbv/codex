{-
  Utility functions for QuickCheck-related stuff
-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.QuickCheck where

import           Control.Applicative

import           Page
import           Markdown

import           System.FilePath
import           Data.List (intersperse)
import           Data.Maybe

-- QuickCheck arguments
data QuickCheckArgs =
  QuickCheckArgs { maxSuccess :: Maybe Int
                 , maxDiscardRatio :: Maybe Int
                 , maxSize :: Maybe Int
                 , randSeed :: Maybe Int
                 } deriving Show


-- get the filepath to Quickcheck properties
-- NB: properties-only (not a module)
getQuickcheckPath :: Page -> Maybe FilePath
getQuickcheckPath Page{..}
  = ((root </> takeDirectory path) </>) <$> lookupFromMeta "quickcheck" meta

{-  = root </> (maybe 
              (replaceExtension path ".hs")
              (takeDirectory path </>)
              (lookupFromMeta "quickcheck" meta))
-}



getQuickcheckArgs :: Page -> QuickCheckArgs
getQuickcheckArgs Page{..} =
  QuickCheckArgs { maxSuccess = lookupFromMeta "maxSuccess" meta,
                   maxSize = lookupFromMeta "maxSize" meta,
                   maxDiscardRatio = lookupFromMeta "maxDiscardRatio" meta,
                   randSeed = lookupFromMeta "randomSeed" meta
                 }

-- setup string for running using suplied arguments
setupArgs :: QuickCheckArgs -> String
setupArgs QuickCheckArgs{..} =
  "stdArgs" ++ 
  args [ fmap (("maxSize="++).show) maxSize, 
         fmap (("maxSuccess="++).show) maxSuccess, 
         fmap (("maxDiscardRatio="++).show)  maxDiscardRatio,
         fmap (\s -> "replay=(mkQCGen " ++ show s ++ ",0)") randSeed
       ] 
  where update :: String -> String
        update [] = ""
        update fields = "{" ++ fields ++ "}"
        args = update . concat . intersperse ", " . catMaybes




{-  
  "stdArgs { maxSuccess = " ++ show maxSuccess ++
  ", maxSize = " ++ show maxSize ++
  ", maxDiscardRatio = " ++ show maxDiscardRatio ++
  maybe "" (\seed -> ", replay = Just (mkQCGen " ++ show seed ++ ",0)")
  optSeed ++
  " }"
-}
