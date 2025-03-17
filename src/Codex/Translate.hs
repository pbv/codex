{-# LANGUAGE OverloadedStrings #-}

module Codex.Translate (translateMarkdown) where

import Network.HTTP.Simple
import Data.Aeson
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import Control.Monad (mzero)

-- Chave da API do DeepL (substitua pela sua)
deepLApiKey :: String
deepLApiKey = "81796376-f600-4c84-ad13-db528d7c9141:fx"

-- URL da API do DeepL
deepLUrl :: String
deepLUrl = "https://api-free.deepl.com/v2/translate"

-- Definição de um novo tipo para extrair a resposta JSON corretamente
newtype TranslationResponse = TranslationResponse { translations :: [Translation] }
  deriving (Show)

newtype Translation = Translation { text :: T.Text }
  deriving (Show)

instance FromJSON TranslationResponse where
  parseJSON (Object v) = TranslationResponse <$> v .: "translations"
  parseJSON _ = mzero

instance FromJSON Translation where
  parseJSON (Object v) = Translation <$> v .: "text"
  parseJSON _ = mzero

-- Função para traduzir um texto Markdown usando DeepL
translateMarkdown :: String -> String -> IO (Maybe String)
translateMarkdown text targetLang = do
  let requestBody = object ["text" .= [T.pack text], "target_lang" .= T.pack targetLang]
      request = setRequestMethod "POST"
              $ setRequestSecure True
              $ setRequestHeader "Authorization" [E.encodeUtf8 (T.pack ("DeepL-Auth-Key " ++ deepLApiKey))]
              $ setRequestHeader "Content-Type" ["application/json"]
              $ setRequestBodyJSON requestBody
              $ parseRequest_ deepLUrl

  response <- httpLBS request
  let responseBody = getResponseBody response
  case decode responseBody of
    Just (TranslationResponse translations) ->
      case translations of
        (Translation translatedText : _) -> return (Just (T.unpack translatedText))
        _ -> return Nothing
    _ -> return Nothing

