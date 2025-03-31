{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric     #-} -- Needed to derive Generic
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-
  Types for various entities
-}

module Codex.Types
  ( -- * types
    UserLogin(..),
    Password(..),
    SubmitId(..),
    Language(..),
    Code(..),
    Page,
    -- re-exports
    Text
  ) where

import           Data.Typeable
import           Data.Hashable

import           Data.String (IsString(..))
import           Data.Text (Text)
import qualified Data.Text as T

import           Data.Int(Int64)
import           Data.ByteString.UTF8 (ByteString)

import           Database.SQLite.Simple.ToField
import           Database.SQLite.Simple.FromField
import           Database.SQLite.Simple.FromRow

import           Data.Configurator.Types
import           Data.Configurator ()

import           Web.Routes.PathInfo
import           Text.Pandoc.Definition -- hiding (Code)


-- | a user login; should uniquely identify the user
newtype UserLogin
  = UserLogin {fromLogin :: Text} deriving (Eq, Ord, Hashable)

newtype Password
  = Password {fromPassword :: ByteString} deriving (Eq,Ord)

-- | submission identifier
newtype SubmitId
  = SubmitId {fromSubmitId :: Int64} deriving (Eq, Ord)

-- | conversion to strings
instance Show UserLogin where
  showsPrec prec (UserLogin uid) = showsPrec prec uid

instance Show SubmitId where
  showsPrec prec (SubmitId sid) = showsPrec prec sid

instance Read SubmitId where
  readsPrec p s = [(SubmitId n, s' ) | (n,s')<-readsPrec p s]

instance PathInfo SubmitId where
  toPathSegments (SubmitId sid) = toPathSegments sid
  fromPathSegments = SubmitId <$> fromPathSegments


-- | language identifier
newtype Language
  = Language {fromLanguage :: Text} deriving (Eq, Ord, Hashable, Typeable) 

-- | program code tagged with language identifier
data Code = Code { codeLang :: !Language
                 , codeText :: !Text
                 } deriving (Eq, Typeable)
  
instance Show Language where
  showsPrec prec (Language l) = showsPrec prec l

instance Configured Language where
  convert v = (Language . T.toLower) <$> convert v


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
  toField = toField . fromSubmitId

instance FromField SubmitId where
  fromField f = SubmitId <$> fromField f

instance FromRow UserLogin where
    fromRow = field

instance ToField Language where
  toField = toField . fromLanguage

instance FromField Language where
  fromField f = Language <$> fromField f

instance FromRow Text where
  fromRow = field


--
-- a Page is a synonym for a Pandoc document 
-- either an active exercise or a passive document (e.g. an index)
--
type Page = Pandoc


