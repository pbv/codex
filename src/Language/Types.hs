{-# LANGUAGE DeriveDataTypeable #-}

module Language.Types (
  Language(..),
  Code(..)
  ) where

import               Data.Maybe
import               Data.Typeable
import               Data.Text(Text)
import               Data.Char
import               Markdown

-- | coding languages
data Language
  = C
  | Cpp
  | Java
  | Python
  | Haskell
    deriving (Eq, Typeable, Show, Read)

-- submission text in some language 
data Code = Code !Language !Text deriving (Eq, Typeable, Read, Show)

instance FromMetaValue Language where
  fromMeta v =  fromMeta v >>= (listToMaybe . map fst . reads . capitalize)

capitalize :: String -> String
capitalize []     = []
capitalize (x:xs) = toUpper x : map toLower xs


