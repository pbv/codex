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
import qualified Text.Pandoc.Builder as P


import           Text.XmlHtml
import           Text.Blaze.Renderer.XmlHtml

import           Data.Maybe
import           Data.List (delete)
import           Data.Foldable(toList)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Read as T

import           Control.Applicative
import           Control.Monad.IO.Class
import           Control.Exception  (IOException)
import           Control.Exception.Lifted  (try)

import           Codex.Types
import           System.FilePath

import           Data.Diff.Myers (diffTexts, Edit(..))



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
               }

blocksToHtml :: [Block] -> [Node]
blocksToHtml blocks = pageToHtml (Pandoc nullMeta blocks)


-- | read a file and parse markdown to a Pandoc document
readMarkdownFile :: MonadIO m => FilePath -> m Pandoc
readMarkdownFile filepath = do
  txt <- liftIO $ T.readFile filepath 
  case parseMarkdown txt of
    Left err  -> return (docError err)
    Right doc -> liftIO $ processIncludes filepath doc

parseMarkdown :: Text -> Either PandocError Pandoc
parseMarkdown txt = runPure $ do
  doc <- readMarkdown pandocReaderOptions txt 
  msgs <- getLog
  return $ docWarnings msgs <> doc  

-- | process all include directives in code blocks 
processIncludes :: FilePath -> Pandoc -> IO Pandoc
processIncludes filepath  
    = walkM include 
    where 
      dir = takeDirectory filepath
      include :: Block -> IO Block
      include (CodeBlock (id, classes, attrs) _) 
        | "include" `elem` classes
        , Just src <- lookup "src" attrs = do
            txt <- T.readFile (dir </> T.unpack src)
            let classes' = delete "include" classes
            let attrs' = delete ("src",src) attrs
            return (CodeBlock (id, classes', attrs') txt)
      include (CodeBlock (id, classes, attrs) _) 
        | "diffs" `elem` classes
        , Just from <- lookup "from" attrs
        , Just to <- lookup "to" attrs = do
            txt1 <- T.readFile (dir </> T.unpack from)
            txt2 <- T.readFile (dir </> T.unpack to)
            let diffs = showDiffs txt1 txt2
            return (Div (id, ["text-diffs"], []) [Plain (toList diffs)])
      -- catch-all
      include block = return block
  


-- | read just the title of a markdown document
readMarkdownTitle :: MonadIO m => FilePath -> m [Inline]
readMarkdownTitle filepath = liftIO $ do
  result <- try (T.readFile filepath)
  return $ case result of
    Left (_ :: IOException) -> brokenLink filepath
    Right txt -> 
      case parseMarkdown txt of
              Left _  -> brokenLink filepath
              Right doc -> fromMaybe (brokenLink filepath) (pageTitle doc) 

brokenLink :: FilePath -> [Inline]
brokenLink path = [Text.Pandoc.Code nullAttr (T.pack path)]


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

-- show text differences as an inline Pandoc object
showDiffs :: Text -> Text -> P.Inlines
showDiffs txt1 txt2 
  =  go 0 (toList $ diffTexts txt1 txt2)
  where 
      go :: Int -> [Edit] -> P.Inlines
      go i [] = pretext (T.drop i txt1)  
      go i (EditDelete from to : rest)
        = let before  = trim i (from-1) txt1 
              deleted = trim from to txt1
          in pretext before <>
             P.strikeout (pretext deleted) <>
             go (to+1) rest 
      go i (EditInsert pos from to : rest) 
         = let before = trim i (pos-1) txt1 
               inserted  = trim from to txt2  
           in pretext before <>
              P.strong (pretext inserted) <>
              go pos rest     

      trim :: Int -> Int -> Text -> Text
      trim i j txt = T.take (j-i+1) (T.drop i txt)

      -- Text preserving spaces and line breaks
      pretext :: Text -> P.Inlines
      pretext txt = walk harden $ P.text $ T.map nbsp txt

      -- change spaces to non-breakable spaces
      nbsp :: Char -> Char
      nbsp ' ' = '\160'
      nbsp x   = x

      -- change softbreaks to hardbreaks
      harden :: P.Inline -> P.Inline
      harden P.SoftBreak = P.LineBreak
      harden i = i
