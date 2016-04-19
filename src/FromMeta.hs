{-
  Convert Pandoc meta values into Haskell types
-} 
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverlappingInstances #-}

module FromMeta where

import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map as Map

import           Data.List (intersperse)

import           Text.Pandoc.Definition
import           Text.Pandoc.Walk

-- import           Control.Applicative ((<$>))
import           Control.Monad
-- import           Control.Monad.Error

-- import           Data.Time.LocalTime
-- import           Data.Time.Format
-- import           System.Locale (defaultTimeLocale)

{-
instance FromMeta LocalTime where
  fromMeta = readLocalTime . T.unpack . metaText 
-- auxiliary definitions
-- parse a local time string 
readLocalTime :: String -> Maybe LocalTime
readLocalTime txt =
  msum [parseTime defaultTimeLocale fmt txt | fmt<-timeFormats] 
  where
    timeFormats = ["%H:%M %d/%m/%Y", "%d/%m/%Y", "%c"]
-}




