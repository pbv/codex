{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-
  Definitions for document and exercise pages
-}
module Codex.Page where

import           Text.Pandoc hiding (Code)
import qualified Text.Pandoc ( Inline(Code) )
import           Text.Pandoc.Walk

import           Text.XmlHtml
import           Text.Blaze.Renderer.XmlHtml

import           Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Read as T

import           Control.Applicative
import           Control.Monad.IO.Class


import           Codex.Types


emptyPage :: Page
emptyPage = Pandoc nullMeta []

-- extract metadata 
pageMeta :: Page -> Meta
pageMeta (Pandoc meta _) = meta

-- extract document description (blocks)
pageDescription :: Page -> [Block]
pageDescription (Pandoc _ blocks) = blocks

pageTitle :: Page -> Maybe [Inline]
pageTitle p
  = case pageTitleBlocks p of
      (Header _ _ h : _) -> Just h
      _                  -> Nothing

-- | lookup title in metadata or take the first header (if any)
pageTitleBlocks :: Page -> [Block]
pageTitleBlocks p
  = case docTitle (pageMeta p) of
      [] -> firstHeader (pageDescription p)
      inlines -> [Header 1 nullAttr inlines]
  where
    firstHeader :: [Block] -> [Block]
    firstHeader blocks = take 1 [block | block@(Header {}) <- blocks]




-- list of accepted languages for an exercise
pageLanguages :: Page -> [Language]
pageLanguages = languages . pageMeta

languages :: Meta -> [Language]
languages meta =
  fromMaybe []
  (lookupFromMeta "languages" meta
   <|>
   do lang <- lookupFromMeta "language" meta
      return [lang])


-- text for submission filled in by default
pageDefaultText :: Page -> Maybe Text
pageDefaultText = lookupFromMeta "code" . pageMeta

pageTester :: Page -> Maybe Text
pageTester = lookupFromMeta "tester" . pageMeta



-- | hide detailed feedback for submissions?
pageShowFeedback :: Page -> Bool
pageShowFeedback = fromMaybe True . lookupFromMeta "feedback" . pageMeta

-- | lock submissions when invalid ?
pageLockInvalid :: Page -> Bool
pageLockInvalid = fromMaybe True . lookupFromMeta "lock" . pageMeta


-- | read from a metadata value
class FromMetaValue a where
  fromMeta :: MetaValue -> Maybe a

instance FromMetaValue Text where
  fromMeta = Just . metaText

instance FromMetaValue Bool where
  fromMeta (MetaBool b) = Just b
  fromMeta (MetaInlines [Str txt])
    | txt == "yes"      = Just True
    | txt == "no"       = Just False
  fromMeta _            = Nothing

instance FromMetaValue Int where
  fromMeta (MetaString s) =
    case T.decimal s of
      (Right (n,"")) -> Just n
      _ -> Nothing
  fromMeta (MetaInlines [Str txt]) =
    case T.decimal txt of
      (Right (n, "")) -> Just n
      _ -> Nothing
  fromMeta _ = Nothing

instance {-# OVERLAPPABLE #-} FromMetaValue a => FromMetaValue [a] where
  fromMeta (MetaList l) = mapM fromMeta l
  fromMeta _            = Nothing

instance {-# OVERLAPPING #-} FromMetaValue String where
  fromMeta = Just . T.unpack . metaText


instance FromMetaValue Language where
  fromMeta v = fmap (Language . T.toLower) (fromMeta v)



-- | lookup from metadata value
lookupFromMeta :: FromMetaValue a => Text -> Meta -> Maybe a
lookupFromMeta tag meta = lookupMeta tag meta >>= fromMeta


inlineText :: Inline -> Text 
inlineText = query f 
   where f :: Inline -> Text
         f (Str s) = s 
         f Space = " "
         f SoftBreak = " "
         f LineBreak = "\n"
         f (Text.Pandoc.Code _ s) = s
         f (Math _ s) = s
         f (RawInline _ s) = s
         f _ = T.empty

inlineString :: Inline -> String
inlineString = T.unpack . inlineText

blockText :: Block -> Text
blockText (Plain l)       = query inlineText l
blockText (Para p)        = query inlineText p
blockText (Header _ _ l)  = query inlineText l
blockText (CodeBlock _ s) = s
blockText (RawBlock  _ s) = s
blockText _               = T.empty

{-
-- collect text in inline and block elements
inlineText :: Inline -> Text
inlineText (Str s)   = s
inlineText Space     = " "
inlineText LineBreak  = "\n"
inlineText (Math _ s) = s
inlineText (Text.Pandoc.Code _ s) = s
inlineText (RawInline _ s) = s
inlineText (Quoted qt l) =  quote <> T.concat (map inlineText l) <> quote
  where quote = case qt of
            SingleQuote -> "\'"
            DoubleQuote -> "\""
inlineText (Emph l)  = T.concat (map inlineText l)
inlineText (Strong l)  = T.concat (map inlineText l)
inlineText (Superscript l)  = T.concat (map inlineText l)
inlineText (Subscript l)  = T.concat (map inlineText l)
inlineText (SmallCaps l)  = T.concat (map inlineText l)
inlineText (Span _ l)  = T.concat (map inlineText l)
inlineText _         = T.empty


-}
{-
-- | collect text from a meta value
metaText :: MetaValue -> Text
metaText (MetaString s) =
  s
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
  intersperse ","  [T.concat [k, ":", metaText v] | (k,v)<- Map.assocs m]
-}

metaText :: MetaValue -> Text
metaText = query f
  where f :: MetaValue -> Text
        f (MetaString s)  = s
        f (MetaInlines l) = T.concat (map inlineText l)
        f (MetaBlocks l)  = T.concat (map blockText l)
        f _               = T.empty





-- | render a page as a list of HTML nodes
pageToHtml :: Page -> [Node]
pageToHtml doc = case runPure (writeHtml5 opts doc) of
  Left err -> error (show err)
  Right nodes -> renderHtmlNodes nodes
  where
    opts = def { writerExtensions = pandocExtensions
               , writerHTMLMathMethod = MathJax "/mathjax"
               -- , writerHighlightStyle = Nothing
               -- , writerHighlightStyle = Just monochrome
               }

blocksToHtml :: [Block] -> [Node]
blocksToHtml blocks = pageToHtml (Pandoc nullMeta blocks)


-- | read a file and parse markdown to a Pandoc document
readMarkdownFile :: MonadIO m => FilePath -> m Pandoc
readMarkdownFile filepath = liftIO $ do
  txt <- T.readFile filepath
  return $ case parseDocument txt of
             Left err -> docError err
             Right (doc,msgs) -> docWarnings msgs  <> doc

parseDocument :: Text -> Either PandocError (Pandoc, [LogMessage])
parseDocument txt = runPure $ do
  doc <- readMarkdown pandocReaderOptions txt
  msgs <- getLog
  return (doc, msgs)


pandocReaderOptions :: ReaderOptions
pandocReaderOptions
  = def { readerExtensions = pandocExtensions
        , readerStripComments = True
        }


-- | report error and warning messages
docError :: PandocError -> Pandoc
docError err
  = Pandoc mempty
    [Div ("", ["errors"], [])
     [Para [Str "ERROR:", Space, Str (T.pack $ show err)]]]

docWarnings :: [LogMessage] -> Pandoc
docWarnings []
  = mempty
docWarnings msgs
  = Pandoc mempty
    [Div ("", ["warnings"], [])
      [Para [Str "WARNING:", Space, Str (T.pack $ show msg)] | msg <- msgs]]




