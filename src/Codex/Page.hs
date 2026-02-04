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

-- import           Text.Pandoc.Highlighting (monochrome)

import           Text.XmlHtml
import           Text.Blaze.Renderer.XmlHtml

import           Data.Maybe
import qualified Data.Map as Map
import           Data.List (delete,intersperse)
import           Data.Foldable(toList)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Read as T

import           Control.Applicative
import           Control.Monad.IO.Class
import           Control.Exception  (IOException, throwIO)
import           Control.Exception.Lifted  (try)

import           Codex.Types
import           System.FilePath

import           Myers.Diff



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
metaText val
  = case val of
        MetaString s -> s
        MetaInlines l -> T.concat (map inlineText l)
        MetaBlocks l -> T.concat (map blockText l)
        MetaBool b -> if b then "true" else "false"
        MetaList l -> T.concat (map metaText l)
        MetaMap kvs -> T.concat [ k <> "," <> metaText v
                                | (k,v)<-Map.assocs kvs]



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
  return $ docWarnings msgs <> cleanupCodeBlocks doc  

-- | ensure all code blocks include some attribute
-- this works around pandoc's default of using indented code blocks
-- with empty attributes
cleanupCodeBlocks :: Pandoc -> Pandoc
cleanupCodeBlocks = walk cleanup
  where
    cleanup :: Block -> Block
    cleanup (CodeBlock attrs code)
      | attrs == nullAttr = CodeBlock ("",["default"],[]) code
    cleanup blk = blk


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
            result <- try (T.readFile (dir </> T.unpack src))
            return $ case result of
              Right txt -> 
                let
                  classes' = delete "include" classes
                  attrs' = delete ("src",src) attrs
                in
                  CodeBlock (id, classes', attrs') txt
              Left (_err :: IOException) -> do
                errorBlock [Plain [Str ("failed to read file " <> src)]]
      include (CodeBlock (id, classes, attrs) _) 
        | "diffs" `elem` classes
        , Just from <- lookup "from" attrs
        , Just to <- lookup "to" attrs = do
            r1 <- try (T.readFile (dir </> T.unpack from))
            r2 <- try (T.readFile (dir </> T.unpack to))
            return $ case (r1, r2) of
              (Right txt1, Right txt2) ->
                let diffs = formatDiffs txt1 txt2
                in (Div (id, ["text-diffs"], []) [Plain (toList diffs)])
              (Left (_err :: IOException), _) ->
                errorBlock [Plain [Str ("failed to read file " <> from)]]
              (_, Left (_err :: IOException)) ->
                errorBlock [Plain [Str ("failed to read file " <> to)]]
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



errorBlock :: [Block] -> Block
errorBlock = Div ("", ["errors"],[]) 

-- Format text differences as an inline Pandoc fragment
-- tries both character and line differences and choose the shorter one
formatDiffs :: Text -> Text -> P.Inlines
formatDiffs txt1 txt2
  = if changes textDiffs <= changes lineDiffs
    then
      hardenBreaks $ mconcat $ map transform $ reorderDiffs textDiffs
    else
      mconcat $ intersperse P.linebreak $ map transform
      $ reorderDiffs lineDiffs
  where
    textDiffs = joinDiffs $ toList $ getTextDiff txt1 txt2
    lineDiffs = toList $ getDiff (T.lines txt1) (T.lines txt2)

    changes :: [Diff a] -> Int
    changes diffs = sum (map count diffs)
    count (First _)  = 1
    count (Second _) = 1
    count (Both _ _) = 0
    
    transform :: Diff Text -> P.Inlines
    transform (First deleted)   = P.strikeout (pretext deleted)
    transform (Second inserted) = P.spanWith ("",["inserted"],[]) (pretext inserted)
    transform (Both common _)   = pretext common


-- change the order of diffs so that deletions come before insertions
reorderDiffs :: [Diff a] -> [Diff a]
reorderDiffs (Second a : First b : rest)
   = First b : reorderDiffs (Second a : rest)
reorderDiffs (a : rest)
  = a : reorderDiffs rest
reorderDiffs []
  = []  

joinDiffs :: Semigroup a => [Diff a] -> [Diff a]
joinDiffs (First a : First b : rest)
  = joinDiffs (First (a<>b) : rest)
joinDiffs (Second a : Second b : rest)
  = joinDiffs (Second (a<>b) : rest)
joinDiffs (a : rest)
  = a : joinDiffs rest
joinDiffs []
  = []


-- preformated text preserving spaces 
pretext :: Text -> P.Inlines
pretext = P.text . T.map nbsp 
  where
    -- change spaces to non-breakable spaces
    nbsp :: Char -> Char
    nbsp ' ' = '\160'
    nbsp x   = x


-- change softbreaks to hardbreaks
hardenBreaks :: P.Inlines -> P.Inlines
hardenBreaks = walk harden
  where
    harden :: P.Inline -> P.Inline
    harden P.SoftBreak = P.LineBreak
    harden i = i


-- ++++++++++++++++++++++++++++++++++++++++++++++++
-- | PI Improvements                              
-- ++++++++++++++++++++++++++++++++++++++++++++++++

-- | Write a Pandoc document to a Markdown file

writeMarkdownFile :: MonadIO m => FilePath -> Pandoc -> m ()
writeMarkdownFile filepath doc = liftIO $ do
  tplText <- runIOorExplode (getTemplate "template.md")
  result <- compileTemplate "template.md" tplText
  case result of
    Left msg ->
      throwIO $ userError msg
    Right tpl -> do
      result <- runIO (writeMarkdown (writerOptions tpl) doc)
      case result of
        Left msg ->
          throwIO $ userError (show msg)
        Right text ->
          T.writeFile filepath text
  
writerOptions tpl
  = def
    { writerTemplate = Just tpl
    , writerExtensions = pandocExtensions
    , writerSetextHeaders = False
    }

-- ++++++++++++++++++++++++++++++++++++++++++++++++
