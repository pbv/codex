{-# LANGUAGE DeriveDataTypeable #-}

module Language.Types (
  Language(..),
  Code(..)
  ) where

import               Data.Typeable
import               Data.Text(Text, toLower)
import               Markdown


newtype Language
  = Language {fromLanguage :: Text}
  deriving (Eq, Typeable, Read, Show)


-- program text in some language 
data Code = Code { codeLang :: !Language
                 , codeText :: !Text
                 } deriving (Eq, Typeable, Read, Show)

instance FromMetaValue Language where
  fromMeta v = fmap (Language . toLower) (fromMeta v)


