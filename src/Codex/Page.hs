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
-- import           Text.Pandoc.Highlighting (monochrome)

import           Text.XmlHtml
import           Text.Blaze.Renderer.XmlHtml

import           Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Read as T
import qualified Data.Map as Map
import           Data.List (intersperse)

import           Control.Applicative
import           Control.Monad.IO.Class


import           Codex.Types
--import           Codex.Policy


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
    firstHeader blocks = take 1 [block | block@(Header _ _ _) <- blocks]




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

blockText :: Block -> Text
blockText (Plain l)       = query inlineText l
blockText (Para p)        = query inlineText p
blockText (Header _ _ l)  = query inlineText l
blockText (CodeBlock _ s) = s
blockText (RawBlock  _ s) = s
blockText _               = T.empty

inlineString :: Inline -> String
inlineString = T.unpack . inlineText


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


-- | render a page as a list of HTML nodes
pageToHtml :: Page -> [Node]
pageToHtml doc = case result of
  Left err -> error (show err)
  Right nodes -> renderHtmlNodes nodes
  where
    result = runPure (writeHtml5 opts doc)
    opts = def { writerExtensions = pandocExtensions
               , writerHTMLMathMethod = MathJax "/mathjax"
               -- , writerHighlightStyle = Nothing
               --, writerHighlightStyle = Just monochrome
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


-- ++++++++++++++++++++++++++++++++++++++++++++++++
-- | PI Improvements                              
-- ++++++++++++++++++++++++++++++++++++++++++++++++

-- | Write a Pandoc document to a Markdown file

writeMarkdownFile :: MonadIO m => FilePath -> Pandoc -> m ()
writeMarkdownFile filepath (Pandoc (Meta metaMap) blocks) = liftIO $ do
  let (blockMetaMap, yamlMetaMap) = Map.partition isMetaBlocks metaMap
      yamlText = renderYamlMeta (Meta yamlMetaMap)

      -- YAML on top + Normal blocks
      contentBlocks = RawBlock "markdown" yamlText : blocks

      -- Render each MetaBlock as a YAML section at the end
      metaBlockTexts = map renderBlockMeta (reverse (Map.toList blockMetaMap))
      metaBlockRawBlocks = map (RawBlock "markdown") metaBlockTexts

      -- Builds the final document
      fullDoc = Pandoc nullMeta (contentBlocks ++ metaBlockRawBlocks)

      writerOpts = def
        { writerExtensions = pandocExtensions
        , writerSetextHeaders = False
        }

  case runPure (writeMarkdown writerOpts fullDoc) of
    Left err  -> error $ "Error writing Markdown file: " ++ show err
    Right txt -> T.writeFile filepath txt

  where
    isMetaBlocks :: MetaValue -> Bool
    isMetaBlocks (MetaBlocks _) = True
    isMetaBlocks _              = False

    renderBlockMeta :: (Text, MetaValue) -> Text
    renderBlockMeta (key, MetaBlocks bs) =
      let blockContent = stringifyBlocks bs
          indented = T.unlines (map ("  " <>) (T.lines blockContent))
      in "---\n" <> key <> ": |\n" <> indented <> "...\n"
    renderBlockMeta _ = ""

    stringifyBlocks :: [Block] -> Text
    stringifyBlocks bs =
      "~~~\n" <> T.concat (map blockToText bs) <> "~~~"

    blockToText :: Block -> Text
    blockToText (Plain ils)    = stringify ils <> "\n"
    blockToText (Para ils)     = stringify ils <> "\n\n"
    blockToText (CodeBlock _ c) = c <> "\n"
    blockToText _              = ""  

    stringify :: [Inline] -> Text
    stringify = T.concat . map inlineToText

    inlineToText :: Inline -> Text
    inlineToText (Str t) = t
    inlineToText Space   = " "
    inlineToText _       = ""

-- | Render YAML from normal metadata
renderYamlMeta :: Meta -> Text
renderYamlMeta (Meta metaMap) =
  let pairs = map renderPair (Map.toList metaMap)
      yaml = "---\n" <> T.unlines pairs <> "..."
  in yaml
  where
    renderPair :: (Text, MetaValue) -> Text
    renderPair (k, v) = k <> ": " <> renderValue v

    renderValue :: MetaValue -> Text
    renderValue (MetaString s)    = s
    renderValue (MetaBool True)   = "true"
    renderValue (MetaBool False)  = "false"
    renderValue (MetaList xs)     = "[" <> T.intercalate ", " (map renderValue xs) <> "]"
    renderValue (MetaInlines ils) = stringify ils
    renderValue (MetaMap m)       = "{" <> T.intercalate ", " (map renderPair (Map.toList m)) <> "}"
    renderValue (MetaBlocks _)    = "<block>"  

    stringify :: [Inline] -> Text
    stringify = T.concat . map inlineToText

    inlineToText :: Inline -> Text
    inlineToText (Str t) = t
    inlineToText Space   = " "
    inlineToText _       = ""


