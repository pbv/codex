{-# LANGUAGE OverloadedStrings, DeriveDataTypeable, DeriveFunctor, EmptyDataDecls #-}
{-
  Types for various entities
-}

module Types where

import           Control.Applicative
import           Data.ByteString.UTF8(ByteString)
import qualified Data.ByteString.UTF8 as B

import           Data.String (IsString(..))
import           Data.Text (Text)
import qualified Data.Text as T

import           Data.Int(Int64)
import           Data.Typeable
import           Data.Time.Clock

import           Database.SQLite.Simple.ToField
import           Database.SQLite.Simple.FromField
import           Database.SQLite.Simple.FromRow

import           Text.Pandoc.Builder hiding (Code)


-- | identifiers
newtype UserID = UserID {fromUID :: ByteString} deriving (Eq, Ord, Show)

newtype ProblemID = ProblemID {fromPID :: ByteString} deriving (Eq, Ord, Show)

newtype SubmitID = SubmitID {fromSID :: Int64} deriving (Eq, Ord, Show)

instance IsString UserID where
  fromString s = UserID (B.fromString s)

instance IsString ProblemID where
  fromString s = ProblemID (B.fromString s)

-- | convertion to/from SQL fields
instance ToField UserID where
  toField (UserID id) = toField id
  
instance FromField UserID where
  fromField f = UserID <$> fromField f

instance ToField ProblemID where
  toField (ProblemID id) = toField id
  
instance FromField ProblemID where
  fromField f = ProblemID <$> fromField f

instance ToField SubmitID where
  toField (SubmitID id) = toField id
  
instance FromField SubmitID where
  fromField f = SubmitID <$> fromField f

instance FromRow UserID where
    fromRow = field

instance FromRow ProblemID where
    fromRow = field

-- | LDAP configuration
data LdapConf = LdapConf { ldapURI :: String
                         , ldapBase :: String
                         , ldapAdmins :: [Text]
                         } 
                deriving (Eq,Show)
                         
-- | Printout configuration
data PrintConf = PrintConf { printEnabled :: Bool 
                           , printHeader :: Text
                           , printOptions :: [String]
                           } 
                deriving (Eq,Show)


-- | code and test fragments 
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




{-
-- | problem tags
type Tag = Text


-- | a type class for collecting tags from problems, etc.
class Tagged a where
    taglist  :: a -> [Tag]

-- check if something is tagged with a single tag
isTagged :: Tagged a => Tag -> a -> Bool
isTagged tag a = tag `elem` taglist a

-- check if something is tagged with a list of tags
hasTags :: Tagged a => [Tag] -> a -> Bool
hasTags tags a = tags `isSublistOf` taglist a
    where isSublistOf xs ys = all (`elem`ys) xs
-}
