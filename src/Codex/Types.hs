{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-
  Types for various entities
-}

module Codex.Types where

import           Data.ByteString.UTF8(ByteString)
import qualified Data.ByteString.UTF8 as B

import           Data.Typeable

import           Data.String (IsString(..))
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import           Data.HashMap.Strict(HashMap)

import           Data.Int(Int64)

import           Database.SQLite.Simple.ToField
import           Database.SQLite.Simple.FromField
import           Database.SQLite.Simple.FromRow


-- | user identifier
newtype UserID
  = UserID {fromUID :: ByteString} deriving (Eq, Ord, Read, Show)

-- | submission identifier
newtype SubmitID
  = SubmitID {fromSID :: Int64} deriving (Eq, Ord, Read, Show)


-- | language identifier
newtype Language
  = Language {fromLanguage :: Text} deriving (Eq, Typeable, Read, Show)

-- | program code tagged with language id
data Code = Code { codeLang :: !Language
                 , codeText :: !Text
                 } deriving (Eq, Typeable, Read, Show)



-- | conversion to text
class ToText a where
  toText :: a -> Text

instance ToText UserID where
  toText (UserID uid) = T.decodeUtf8 uid
    --T.pack (B.toString uid)

instance ToText SubmitID where
  toText (SubmitID sid) = T.pack (show sid)

-- | conversion from strings
instance IsString UserID where
  fromString s = UserID (B.fromString s)


-- | convertion to/from SQL fields
instance ToField UserID where
  toField (UserID uid) = toField uid

instance FromField UserID where
  fromField f = UserID <$> fromField f

instance ToField SubmitID where
  toField (SubmitID sid) = toField sid

instance FromField SubmitID where
  fromField f = SubmitID <$> fromField f

instance FromRow UserID where
    fromRow = field



-- | LDAP configuration
data LdapConf = LdapConf { ldapURI :: String
                         , ldapBase :: String
                         , ldapMap :: HashMap Text Text  -- ^ attribute mapping
                         }
                deriving Show

{-
-- | Printout configuration
data PrintConf = PrintConf { printEnabled :: Bool
                           , printHeader :: Text
                           , printOptions :: [String]
                           }
                deriving Show


-}
