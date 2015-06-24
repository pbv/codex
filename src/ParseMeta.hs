{-
  Parse Pandoc meta values 
-} 
{-# LANGUAGE OverloadedStrings, TypeSynonymInstances, FlexibleInstances, OverlappingInstances #-}
{- trust me, I'm a doctor! -}
module ParseMeta where

import           Data.Text (Text)
import qualified Data.Text as T

import           Text.Pandoc.Definition
import           Text.Pandoc.Walk

import           Control.Applicative ((<$>))
import           Control.Monad
import           Control.Monad.Error

import           Data.Time.LocalTime
import           Data.Time.Format
import           System.Locale (defaultTimeLocale)



-- | parse from a metadata value
class ParseMeta a where
  parseMeta :: MetaValue -> Either String a

  
fromMeta :: ParseMeta a => MetaValue -> Maybe a
fromMeta v = case parseMeta v of
  Left _ -> Nothing
  Right r -> Just r


instance ParseMeta Text where
  parseMeta v = return (metaText v)

instance ParseMeta String where
  parseMeta v = return (T.unpack $ metaText v)


instance ParseMeta LocalTime where
  parseMeta v = parseLocalTime (T.unpack $ metaText v)

-- parse a local time string 
parseLocalTime :: String -> Either String LocalTime
parseLocalTime txt =
  case msum [parseTime defaultTimeLocale fmt txt | fmt<-timeFormats] of
    Just t -> return t
    Nothing -> throwError ("expected time " ++ show timeFormats ++ ", got " ++ show txt)
  where
    timeFormats = ["%H:%M %d/%m/%Y", "%d/%m/%Y", "%c"]

instance ParseMeta Bool where
  parseMeta (MetaBool b) = return b
  parseMeta v = throwError ("expected boolean, got " ++ show v)
  

instance ParseMeta a => ParseMeta [a] where
  parseMeta (MetaList l) = mapM parseMeta l
  parseMeta v = throwError ("expected list, got " ++ show v)




-- collect text in inline and block elements
inlineText :: Inline -> Text
inlineText (Str s)   = T.pack s
inlineText Space     = T.singleton ' '
inlineText LineBreak = T.singleton '\n'
inlineText (Math _ s)= T.pack s
inlineText (Code _ s) = T.pack s
inlineText (RawInline _ s) = T.pack s
inlineText _         = T.empty

blockText :: Block -> Text
blockText (Plain l)       = query inlineText l
blockText (Para p)        = query inlineText p
blockText (Header _ _ l)  = query inlineText l
blockText (CodeBlock _ s) = T.pack s
blockText (RawBlock  _ s) = T.pack s
blockText _               = T.empty


inlineString = T.unpack . inlineText


-- | collect text from a meta value
metaText :: MetaValue -> Text
metaText (MetaString s) = T.pack s
metaText (MetaInlines l) = T.concat (map inlineText l)
metaText (MetaBlocks l) = T.concat (map blockText l)
metaText _              = T.empty
