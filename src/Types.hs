{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
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

import           SafeExec

-- | identifiers
newtype UserID = UserID ByteString deriving (Eq, Ord, Read, Show) 

newtype ProblemID = ProblemID ByteString deriving (Eq, Ord, Read, Show) 

newtype SubmitID = SubmitID Int64 deriving (Eq, Ord, Read, Show) 


-- | conversion to text
class ToText a where
  toText :: a -> Text

instance ToText UserID where
  toText (UserID uid) = T.pack (B.toString uid)

instance ToText ProblemID where
  toText (ProblemID pid) = T.pack (B.toString pid)

instance ToText SubmitID where
  toText (SubmitID sid) = T.pack (show sid)

-- | conversion from strings
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

-- | Haskell configuration
data HaskellConf = HaskellConf { haskellExec :: !FilePath
                               , haskellSfConf :: !SafeExecConf
                               } deriving (Eq, Show)


-- | Python configuration
data PythonConf = PythonConf { pythonExec :: !FilePath
                             , pythonScript :: !FilePath
                             , pythonSfConf :: !SafeExecConf
                             } deriving (Eq, Show)




-- fromCode :: Code lang -> Text
-- fromCode (Code text) = text

{-
-- convertions to/from strings
instance IsString (Code lang) where
  fromString s = Code (T.pack s)

-- convertions to/from SQL fields
instance ToField (Code lang) where
  toField (Code txt) = toField txt

instance FromField (Code lang) where
  fromField f = Code <$> fromField f
-}


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
