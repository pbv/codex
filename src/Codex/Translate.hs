{-# LANGUAGE OverloadedStrings #-}

module Codex.Translate (translateMarkdown) where

import Network.HTTP.Simple
import Data.Aeson
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import qualified Data.Text.IO as TIO
import Control.Monad (mzero)
import Data.List (foldl', minimumBy)
import Data.Maybe (mapMaybe)

-- DeepL API Key 
deepLApiKey :: String
deepLApiKey = "81796376-f600-4c84-ad13-db528d7c9141:fx"

-- DeepL API URL
deepLUrl :: String
deepLUrl = "https://api-free.deepl.com/v2/translate"

-- New type to extract the JSON response
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


-- Function to replace protected blocks with placeholders
replaceProtectedBlocks :: T.Text -> (T.Text, [(T.Text, T.Text)])
replaceProtectedBlocks text =
    let patterns = [("---", ["...", "---"]), ("~~~", ["~~~"]), ("```", ["```"]), ("`", ["`"]), ("$", ["$"])] -- Delimiters
    in go text patterns 1 []

  where
    go :: T.Text -> [(T.Text, [T.Text])] -> Int -> [(T.Text, T.Text)] -> (T.Text, [(T.Text, T.Text)])
    go txt [] _ placeholders = (txt, reverse placeholders)  -- When there are no more patterns, return the result
    go txt ((startMarker, endMarkers):ps) counter placeholders =
        case findNextBlock txt startMarker endMarkers counter [] of
            (newTxt, newPlaceholders, newCounter) -> go newTxt ps newCounter (newPlaceholders ++ placeholders)

    -- Finds and replaces all blocks of a pattern in text
    findNextBlock :: T.Text -> T.Text -> [T.Text] -> Int -> [(T.Text, T.Text)] -> (T.Text, [(T.Text, T.Text)], Int)
    findNextBlock txt startMarker endMarkers counter acc =
        case T.breakOn startMarker txt of
            (before, rest) | T.null rest -> (txt, acc, counter)  -- No delimiters found
            (_, rest) ->
                let rest' = T.drop (T.length startMarker) rest
                    chosenEndMarker = findFirstEndMarker rest' endMarkers
                in case chosenEndMarker of
                     Nothing -> (txt, acc, counter)  -- If there is no final delimiter, it does not replace
                     Just endMarker ->
                         let (content, afterContent) = T.breakOn endMarker rest'
                         in if T.null afterContent
                            then (txt, acc, counter)  -- If there is no final delimiter, it does not replace
                            else 
                                let placeholder = T.pack $ "{{PLACEHOLDER" ++ show counter ++ "}}"
                                    fullMatch = startMarker <> content <> endMarker
                                    newTxt = T.replace fullMatch placeholder txt
                                in findNextBlock newTxt startMarker endMarkers (counter + 1) ((fullMatch, placeholder) : acc)

    -- Find which closing delimiter appears first in the text
    findFirstEndMarker :: T.Text -> [T.Text] -> Maybe T.Text
    findFirstEndMarker txt markers =
        let positions = [(m, T.breakOn m txt) | m <- markers]
            validPositions = [(m, T.length before) | (m, (before, after)) <- positions, not (T.null after)]
        in if null validPositions
           then Nothing
           else Just (fst (minimumBy (\(_, i1) (_, i2) -> compare i1 i2) validPositions))


-- Function to restore placeholders with original values
restoreProtectedBlocks :: T.Text -> [(T.Text, T.Text)] -> T.Text
restoreProtectedBlocks text placeholders =
  foldl' (\acc (orig, ph) -> T.replace ph orig acc) text placeholders

-- Function to translate Markdown text using DeepL
translateMarkdown :: String -> String -> IO (Maybe String)
translateMarkdown text targetLang = do
  let (safeText, placeholders) = replaceProtectedBlocks (T.pack text)
      requestBody = object ["text" .= [safeText], "target_lang" .= T.pack targetLang]
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
        (Translation translatedText : _) -> return (Just (T.unpack (restoreProtectedBlocks translatedText placeholders)))
        _ -> return Nothing
    _ -> return Nothing
