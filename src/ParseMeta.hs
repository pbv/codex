{-
  Parse Pandoc meta values 
-} 
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
  parseMeta v = return (query inlineText v)

-- instance ParseMeta String where
--  parseMeta v = return (query inlineString v)

instance ParseMeta Bool where
  parseMeta (MetaBool b)  = return b
  parseMeta _             = throwError "parse error on boolean value"
  

instance ParseMeta LocalTime where
  parseMeta v = (T.unpack <$> parseMeta v) >>= parseLocalTime

-- parse a local time string 
parseLocalTime :: String -> Either String LocalTime
parseLocalTime txt =
  case msum [parseTime defaultTimeLocale fmt txt | fmt<-timeFormats] of
    Just t -> return t
    Nothing -> throwError ("parse error on time value: " ++ show txt)
  where
    timeFormats = ["%H:%M %d/%m/%Y", "%d/%m/%Y", "%c"]


instance ParseMeta a =>  ParseMeta [a] where
  parseMeta (MetaList l) = mapM parseMeta l
  parseMeta _            = throwError "parse error on list value"




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

