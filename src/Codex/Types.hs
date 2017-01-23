{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-
  Types for various entities
-}

module Codex.Types
  ( -- * types
    UserLogin(..), SubmitId(..),  Language(..), Code(..),
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

import           Data.Configurator.Types
import           Data.Configurator ()

-- | a user login; should uniquely identify the user
newtype UserLogin
  = UserLogin {fromLogin :: Text} deriving (Eq, Ord)


-- | submission identifier
newtype SubmitId
  = SubmitId {fromSid :: Int64} deriving (Eq, Ord)


-- | conversion to strings
instance Show UserLogin where
  showsPrec prec (UserLogin uid) = showsPrec prec uid

instance Show SubmitId where
  showsPrec prec (SubmitId sid) = showsPrec prec sid

-- | language identifier
newtype Language
  = Language {fromLanguage :: Text} deriving (Eq, Typeable) --, Read, Show)

-- | program code tagged with language identifier
data Code = Code { codeLang :: !Language
                 , codeText :: !Text
                 } deriving (Eq, Typeable) --, Read, Show)
  
instance Show Language where
  showsPrec prec (Language l) = showsPrec prec l

instance Configured Language where
  convert v = (Language . T.toLower) <$> convert v


-- | type class to overload conversion to text
class ToText a where
  toText :: a -> Text

instance ToText Text where
  toText = id

instance ToText UserLogin where
  toText = fromLogin

instance ToText SubmitId where
  toText = T.pack . show . fromSid

instance ToText Language where
  toText = fromLanguage


-- | conversion from strings
instance IsString UserLogin where
  fromString = UserLogin . T.pack

instance IsString Language where
  fromString = Language . T.pack
  
-- | convertion to/from SQL fields
instance ToField UserLogin where
  toField = toField . fromLogin

instance FromField UserLogin where
  fromField f = UserLogin <$> fromField f

instance ToField SubmitId where
  toField = toField . fromSid

instance FromField SubmitId where
  fromField f = SubmitId <$> fromField f

instance FromRow UserLogin where
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
