{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Codex.Translate (translateMarkdown) where

import           Network.HTTP.Simple
import           Data.Aeson
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import           Control.Monad (mzero)
import           Text.Pandoc.Definition
import qualified Text.Pandoc.Builder as P
import           Text.Pandoc.Walk (walkM)
import           Data.Char(isSpace)

import Codex.Types (Page)
import Codex.Utils
import Codex.Application


-- | Response types
newtype TranslationResponse = TranslationResponse [Translation]

newtype Translation = Translation { translationText :: Text }

instance FromJSON TranslationResponse where
  parseJSON (Object v)  = TranslationResponse <$> v .: "translations"
  parseJSON _ = mzero

instance FromJSON Translation where
  parseJSON (Object v)  = Translation <$> v .: "text" 
  parseJSON _ = mzero


-- | Main translation function
translateMarkdown :: Page -> String -> Codex Page
translateMarkdown (Pandoc meta blocks) targetLang = do
  translatedBlocks <- walkM (translateInlines targetLang) blocks
  return $ Pandoc meta translatedBlocks 


-- | ++++++++++ INLINE SPLIT & JOIN ++++++++++

data InlineChunk
  = Translatable Text
  | Untranslatable Inline
  deriving Show

-- | Translates blocks of text while preserving others
translateInlines :: String -> [Inline] -> Codex [Inline]
translateInlines lang inlines = do
  translatedChunks <- mapM translateChunk (splitInlines inlines)
  return $ P.toList $ mconcat translatedChunks
  where
    translateChunk :: InlineChunk -> Codex P.Inlines
    translateChunk (Translatable txt) = do     
      translated <- sendTranslationRequest txt lang
      -- work around DeepL bug that sometimes removes trailing spaces
      return (P.text translated <>
              if isSpace (T.last txt) then P.space else mempty
             )
    translateChunk (Untranslatable inline) = return (P.singleton inline)

-- | Splits inlines into chunks that should or should not be translated
splitInlines :: [Inline] -> [InlineChunk]
splitInlines [] = []
splitInlines (Str s:xs)
  = go xs s
  where
    go [] acc = [Translatable acc]
    go (x:xs) acc
      | Just s' <- toText x = go xs (acc<>s')
      | otherwise = Translatable acc : Untranslatable x : splitInlines xs
splitInlines (x:xs) = Untranslatable x : splitInlines xs

-- text inlines can should be collected and translated
toText :: Inline -> Maybe Text
toText (Str s)   = Just s
toText Space     = Just " "
toText SoftBreak = Just " "
toText LineBreak = Just "\n"
toText _         = Nothing


-- | ++++++++++ DEEPL API REQUEST ++++++++++ 

sendTranslationRequest :: Text -> String -> Codex Text
sendTranslationRequest text targetLang = do
  conf <- getDeepLConfig
  let request = prepareRequest conf targetLang text
  response <- httpLBS request
  case decode (getResponseBody response) of
     Just (TranslationResponse (t:_)) -> return (translationText t)
     _ -> return text  

prepareRequest :: DeepLConfig -> String -> Text -> Request
prepareRequest DeepLConfig{..} targetLang text =
  let requestBody
        = object (maybe [] (\lang -> ["source_lang" .=  lang]) defaultLanguage
                  ++
                  [ "target_lang" .= targetLang
                  , "text" .= [text]
                  ])
    in setRequestMethod "POST"
         $ setRequestSecure True
         $ setRequestHeader "Authorization"
                     [E.encodeUtf8 (T.pack ("DeepL-Auth-Key " ++ deepLAPIKey))]
         $ setRequestHeader "Content-Type" ["application/json"]
         $ setRequestBodyJSON requestBody
         $ parseRequest_ deepLURL


