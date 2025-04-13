{-# LANGUAGE OverloadedStrings #-}

module Codex.Translate (translateMarkdown) where

import Network.HTTP.Simple
import Data.Aeson
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import Control.Monad (mzero)
import Text.Pandoc.Definition
import Text.Pandoc.Walk (walkM)
import Codex.Page (inlineText)
import Codex.Types(Page)

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
  translatedBlocks <- mapM (walkM translateBlock) blocks
  return $ Just (Pandoc meta translatedBlocks)
  where
    translateBlock :: Block -> IO Block
    translateBlock (Plain inlines) = Plain <$> translateInlines inlines
    translateBlock (Para inlines) = Para <$> translateInlines inlines
    translateBlock (Header level attr inlines) = Header level attr <$> translateInlines inlines
    translateBlock (OrderedList attr items) = OrderedList attr <$> mapM translateListItem items
    translateBlock (BulletList items) = BulletList <$> mapM translateListItem items
    translateBlock x = return x  -- Leave other blocks unchanged

    translateListItem :: [Block] -> IO [Block]
    translateListItem = mapM (walkM translateInlineBlock)

    translateInlineBlock :: Inline -> IO Inline
    translateInlineBlock (Str text) = Str <$> translateText text
    translateInlineBlock x = return x

    translateInlines :: [Inline] -> IO [Inline]
    translateInlines = mapM translateInlineBlock

    translateText :: T.Text -> IO T.Text
    translateText text
      | T.null text = return text
      | otherwise = sendTranslationRequest text targetLang

-- API communication
sendTranslationRequest :: T.Text -> String -> IO T.Text
sendTranslationRequest text targetLang = do
  request <- prepareRequest text targetLang
  response <- httpLBS request
  case decode (getResponseBody response) of
    Just (TranslationResponse (Translation t:_)) -> return t
    _ -> return text  -- Fallback to original text if translation fails

prepareRequest :: T.Text -> String -> IO Request
prepareRequest text targetLang = do
  let requestBody = object ["text" .= [text], "target_lang" .= T.pack targetLang]
  return $ setRequestMethod "POST"
         $ setRequestSecure True
         $ Network.HTTP.Simple.setRequestHeader "Authorization" [E.encodeUtf8 (T.pack ("DeepL-Auth-Key " ++ deepLApiKey))]
         $ Network.HTTP.Simple.setRequestHeader "Content-Type" ["application/json"]
         $ setRequestBodyJSON requestBody
         $ parseRequest_ deepLUrl