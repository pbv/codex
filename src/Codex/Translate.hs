{-# LANGUAGE OverloadedStrings #-}

module Codex.Translate (translateMarkdown) where

import Network.HTTP.Simple
import Data.Aeson
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import Control.Monad (mzero)
import Data.List (span)
import Text.Pandoc.Definition
import Text.Pandoc.Walk (walkM)
import Codex.Types (Page)

-- DeepL API Key and URL
deepLApiKey :: String
deepLApiKey = "81796376-f600-4c84-ad13-db528d7c9141:fx"

deepLUrl :: String
deepLUrl = "https://api-free.deepl.com/v2/translate"

-- Response types
newtype TranslationResponse = TranslationResponse { translations :: [Translation] }
newtype Translation = Translation { text :: T.Text }

instance FromJSON TranslationResponse where
  parseJSON (Object v) = TranslationResponse <$> v .: "translations"
  parseJSON _ = mzero

instance FromJSON Translation where
  parseJSON (Object v) = Translation <$> v .: "text"
  parseJSON _ = mzero

-- Main translation function
translateMarkdown :: Page -> String -> IO (Maybe Page)
translateMarkdown (Pandoc meta blocks) targetLang = do
  translatedBlocks <- mapM (walkM (translateBlock targetLang)) blocks
  return $ Just (Pandoc meta translatedBlocks)

translateBlock :: String -> Block -> IO Block
translateBlock lang (Plain inlines) = Plain <$> translateInlineGroup lang inlines
translateBlock lang (Para inlines) = Para <$> translateInlineGroup lang inlines
translateBlock lang (Header level attr inlines) = Header level attr <$> translateInlineGroup lang inlines
translateBlock lang (OrderedList attr items) = OrderedList attr <$> mapM (mapM (walkM (translateBlock lang))) items
translateBlock lang (BulletList items) = BulletList <$> mapM (mapM (walkM (translateBlock lang))) items
translateBlock _ blk = return blk

-- === INLINE TRANSLATION LOGIC === --

data InlineChunk = Translatable [Inline] | Untranslatable [Inline]

-- Divide os inlines em blocos que devem ou não ser traduzidos
chunkInlines :: [Inline] -> [InlineChunk]
chunkInlines [] = []
chunkInlines xs = go xs
  where
    go [] = []
    go (x:xs) =
      let (grp, rest) = span (sameKind x) (x:xs)
       in (if isTranslatable x then Translatable else Untranslatable) grp : go rest

    sameKind a b = isTranslatable a == isTranslatable b
    isTranslatable (Str _) = True
    isTranslatable Space = True
    isTranslatable SoftBreak = True
    isTranslatable LineBreak = True
    isTranslatable _ = False

-- Traduz blocos de texto, preservando os outros
translateInlineGroup :: String -> [Inline] -> IO [Inline]
translateInlineGroup lang inlines = do
  translatedChunks <- mapM translateChunk (chunkInlines inlines)
  return $ mergeChunksWithSpacing translatedChunks
  where
    translateChunk :: InlineChunk -> IO [Inline]
    translateChunk (Translatable is) = do
      let txt = inlinesToText is
      translated <- sendTranslationRequest txt lang
      return $ textToInlinesPreserveSpaces translated
    translateChunk (Untranslatable is) = return is

-- Junta os blocos de Inline, inserindo espaços inteligentes quando necessário
mergeChunksWithSpacing :: [[Inline]] -> [Inline]
mergeChunksWithSpacing = foldr insertChunk []
  where
    insertChunk [] acc = acc
    insertChunk chunk [] = chunk
    insertChunk chunk acc@(a:_)
      | needsSpace (last chunk) a = chunk ++ (Space : acc)
      | otherwise = chunk ++ acc

    needsSpace :: Inline -> Inline -> Bool
    needsSpace x y =
      not (endsWithSpace x || startsWithSpace y) &&
      (isTextual x && not (isTextual y) || not (isTextual x) && isTextual y)

    isTextual (Str _) = True
    isTextual Space = True
    isTextual SoftBreak = True
    isTextual LineBreak = True
    isTextual _ = False

    endsWithSpace (Str t) = T.isSuffixOf " " t
    endsWithSpace Space = True
    endsWithSpace _ = False

    startsWithSpace (Str t) = T.isPrefixOf " " t
    startsWithSpace Space = True
    startsWithSpace _ = False

-- Concatena o texto dos inlines textuais
inlinesToText :: [Inline] -> T.Text
inlinesToText = T.concat . map inlineToText
  where
    inlineToText (Str t) = t
    inlineToText Space = " "
    inlineToText SoftBreak = " "
    inlineToText LineBreak = "\n"
    inlineToText _ = ""

-- Divide o texto traduzido em inlines, preservando espaços
textToInlinesPreserveSpaces :: T.Text -> [Inline]
textToInlinesPreserveSpaces = map Str . splitPreservingSpaces

-- Divide texto mantendo espaços como elementos
splitPreservingSpaces :: T.Text -> [T.Text]
splitPreservingSpaces txt
  | T.null txt = []
  | otherwise = case T.span (not . isSpaceLike) txt of
      (w, rest) ->
        let (spaces, rest') = T.span isSpaceLike rest
         in [w | not (T.null w)] ++ [spaces | not (T.null spaces)] ++ splitPreservingSpaces rest'
  where
    isSpaceLike c = c == ' ' || c == '\n' || c == '\t'

-- === DEEPL API === --

sendTranslationRequest :: T.Text -> String -> IO T.Text
sendTranslationRequest text targetLang = do
  request <- prepareRequest text targetLang
  response <- httpLBS request
  case decode (getResponseBody response) of
    Just (TranslationResponse (Translation t:_)) -> return t
    _ -> return text  -- fallback

prepareRequest :: T.Text -> String -> IO Request
prepareRequest text targetLang = do
  let requestBody = object ["text" .= [text], "target_lang" .= T.pack targetLang]
  return $ setRequestMethod "POST"
         $ setRequestSecure True
         $ setRequestHeader "Authorization" [E.encodeUtf8 (T.pack ("DeepL-Auth-Key " ++ deepLApiKey))]
         $ setRequestHeader "Content-Type" ["application/json"]
         $ setRequestBodyJSON requestBody
         $ parseRequest_ deepLUrl

