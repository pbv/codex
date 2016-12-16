{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Markdown where

import           Text.Pandoc hiding (Code)

import qualified Text.Pandoc ( Inline(Code) )
import           Text.Pandoc.Walk
import           Text.XmlHtml 
import           Text.Blaze.Renderer.XmlHtml

import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map as Map
-- import           Data.Monoid

import           Data.List (intersperse)
-- import           System.IO.Error (ioError, userError)
-- import           Types



-- | read from a metadata value
class FromMetaValue a where
  fromMeta :: MetaValue -> Maybe a
  
instance FromMetaValue Text where
  fromMeta = Just . metaText

instance FromMetaValue Bool where
  fromMeta (MetaBool b) = Just b
  fromMeta _            = Nothing

instance FromMetaValue Int where
  fromMeta (MetaString s) =
    case reads s of
      ((n,""):_) -> Just n
      _ -> Nothing
  fromMeta _ = Nothing

instance {-# OVERLAPPABLE #-} FromMetaValue a => FromMetaValue [a] where
  fromMeta (MetaList l) = mapM fromMeta l
  fromMeta _            = Nothing
 
instance {-# OVERLAPPING #-} FromMetaValue String where
  fromMeta = Just . T.unpack . metaText 


-- | lookup from metadata value
lookupFromMeta :: FromMetaValue a => String -> Meta -> Maybe a
lookupFromMeta tag meta = lookupMeta tag meta >>= fromMeta


-- collect text in inline and block elements
inlineText :: Inline -> Text
inlineText (Str s)   = T.pack s
inlineText Space     = T.singleton ' '
inlineText LineBreak = T.singleton '\n'
inlineText (Math _ s)= T.pack s
inlineText (Text.Pandoc.Code _ s) = T.pack s
inlineText (RawInline _ s) = T.pack s
inlineText _         = T.empty

blockText :: Block -> Text
blockText (Plain l)       = query inlineText l
blockText (Para p)        = query inlineText p
blockText (Header _ _ l)  = query inlineText l
blockText (CodeBlock _ s) = T.pack s
blockText (RawBlock  _ s) = T.pack s
blockText _               = T.empty

inlineString :: Inline -> String
inlineString = T.unpack . inlineText


-- | collect text from a meta value
metaText :: MetaValue -> Text
metaText (MetaString s) =
  T.pack s
metaText (MetaBool b) =
  T.pack (show b)
metaText (MetaInlines l) =
  T.concat (map inlineText l)
metaText (MetaBlocks l) =
  T.concat (map blockText l)
metaText (MetaList l) =
  T.concat (intersperse "," $ map metaText l)
metaText (MetaMap m) =
  T.concat $
  intersperse ","  $
  [ T.concat [T.pack k, ":", metaText v] | (k,v)<- Map.assocs m]




-- | convert Pandoc to HTML nodes
pandocToHtml :: Pandoc -> [Node]
pandocToHtml = renderHtmlNodes . writeHtml myWriterOptions
  where
    myWriterOptions :: WriterOptions
    myWriterOptions = def { writerExtensions = pandocExtensions
                          , writerHTMLMathMethod = MathJax "/mathjax",
                            writerHighlight = True
                          }



blocksToHtml :: [Block] -> [Node]
blocksToHtml = pandocToHtml . makeDoc

makeDoc :: [Block] -> Pandoc
makeDoc = Pandoc mempty 

-- inlinesToHtml :: Inlines -> [Node]
--inlinesToHtml = blocksToHtml . plain

-- | read a file and parse markdown to a Pandoc document
readMarkdownFile :: FilePath -> IO Pandoc
readMarkdownFile fp = do
  r <- fmap (readMarkdown opts) (readFile fp)
  case r of
    Left err -> ioError (userError $ show err)
    Right doc -> return doc
  where
    opts = def { readerExtensions = pandocExtensions
               , readerSmart = True 
               }

