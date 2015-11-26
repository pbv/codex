{-# LANGUAGE OverloadedStrings, EmptyDataDecls #-}
module Language where

import           Control.Applicative
import           Data.String
import           Data.Text (Text)
import qualified Data.Text as T
import           Database.SQLite.Simple.ToField
import           Database.SQLite.Simple.FromField


-- | Code fragments tagged with language  
newtype Code lang = Code { fromCode :: Text } deriving (Eq,Show)

{-
data Language = C
              | CPP
              | Java
              | Python
              | Haskell
              | Doctest
              | QuickCheck
              | JUnit
                deriving (Eq, Show, Read)
-}

data Python
data Doctest  


instance IsString (Code lang) where
  fromString s = Code (T.pack s)
  
toCode :: Text -> Code lang
toCode = Code 

-- | convertion to/from SQL fields
instance ToField (Code lang) where
  toField (Code txt) = toField txt

instance FromField (Code lang) where
  fromField f = toCode <$> fromField f

