{-# LANGUAGE OverloadedStrings #-}
module Language where

import           Data.String
import           Data.Text (Text)
import qualified Data.Text as T

newtype Code lang = Code { fromCode :: Text } deriving (Eq,Show)

instance IsString (Code lang) where
  fromString s = Code (T.pack s)
  

toCode :: Text -> Code lang
toCode = Code

-- phantom types for tagging Code 
data Python
data Doctest

{-
data Haskell
data QuickCheck  
-}

