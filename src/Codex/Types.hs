{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-
  Types for various entities
-}

module Codex.Types
  ( -- * types
    UserId(..), SubmitId(..),  Language(..), Code(..),
    -- * typeclass
    Text, toText
  ) where

import           Data.Typeable

import           Data.String (IsString(..))
import           Data.Text (Text)
import qualified Data.Text as T

import           Data.Int(Int64)

import           Database.SQLite.Simple.ToField
import           Database.SQLite.Simple.FromField
import           Database.SQLite.Simple.FromRow

import           Snap.Snaplet.Auth (UserId(..))


-- | user identifiers
-- UserId defined in Snap.Snaplet.Auth

-- | submission identifier
newtype SubmitId
  = SubmitId {unSid :: Int64} deriving (Eq, Ord, Read, Show)

-- | language identifier
newtype Language
  = Language {fromLanguage :: Text} deriving (Eq, Typeable, Read, Show)

-- | program code tagged with language identifier
data Code = Code { codeLang :: !Language
                 , codeText :: !Text
                 } deriving (Eq, Typeable, Read, Show)
  
  
-- | type class to overload conversion to text
class ToText a where
  toText :: a -> Text

instance ToText Text where
  toText = id

instance ToText UserId where
  toText = unUid

instance ToText SubmitId where
  toText = T.pack . show . unSid

instance ToText Language where
  toText = fromLanguage


-- | conversion from strings
instance IsString UserId where
  fromString = UserId . T.pack

instance IsString Language where
  fromString = Language . T.pack
  
-- | convertion to/from SQL fields
instance ToField UserId where
  toField = toField . unUid

instance FromField UserId where
  fromField f = UserId <$> fromField f

instance ToField SubmitId where
  toField = toField . unSid

instance FromField SubmitId where
  fromField f = SubmitId <$> fromField f

instance FromRow UserId where
    fromRow = field

instance ToField Language where
  toField = toField . fromLanguage

instance FromField Language where
  fromField f = Language <$> fromField f


{-
-- | Printout configuration
data PrintConf = PrintConf { printEnabled :: Bool
                           , printHeader :: Text
                           , printOptions :: [String]
                           }
                deriving Show


-}
