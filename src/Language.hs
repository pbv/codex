{-# LANGUAGE OverloadedStrings #-}
module Language where

import           Control.Applicative
import           Data.String
import           Data.Text (Text)
import qualified Data.Text as T
import           Database.SQLite.Simple.ToField
import           Database.SQLite.Simple.FromField

-- | Python configuration
data PythonConf = PythonConf { pythonExec :: !FilePath
                             , pythonScript :: !FilePath
                             } deriving (Eq, Show)

-- | Haskell configuration
data HaskellConf = HaskellConf { haskellExec :: !FilePath
                               } deriving (Eq, Show)



-- | code fragments 
newtype Code = Code { fromCode :: Text } deriving (Eq,Show)

toCode :: Text -> Code 
toCode = Code 

newtype Tests = Tests { fromTests :: Text } deriving (Eq,Show)

toTests :: Text -> Tests 
toTests = Tests
  

instance IsString Code where
  fromString s = Code (T.pack s)

instance IsString Tests where
  fromString s = Tests (T.pack s)


-- | convertion to/from SQL fields
instance ToField Code where
  toField (Code txt) = toField txt

instance FromField Code where
  fromField f = Code <$> fromField f


