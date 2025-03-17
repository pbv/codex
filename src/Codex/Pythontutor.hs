{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Codex.Pythontutor (
  pythontutorSplices
  ) where

import           Data.Text (Text)
import qualified Data.Text as T

import qualified Data.ByteString.UTF8 as B
import qualified Text.Regex as RE

import           Control.Monad (guard)
import qualified Data.Map as Map

import           Heist.Splices     as I
import qualified Heist.Interpreted as I

import           Data.Map.Syntax

import           Codex.Tester.Result
import           Codex.Submission.Types
import           Codex.Page
import           Codex.Types
import           Codex.Utils



pythontutorSplices :: Page -> Submission -> ISplices
pythontutorSplices page sub
  =  case (getTutorURL page >>= makeTutorLink sub) of
      Just url -> do
        "if-pythontutor" ## I.ifElseISplice True
        "pythontutor-url" ## I.textSplice url
      Nothing -> do
        "if-pythontutor" ## I.ifElseISplice False        
        "pythontutor-url" ## return []


-- | get the base URL for the Pythontutor site
-- e.g. http://www.pythontutor.com/c.html
getTutorURL :: Page  -> Maybe Text
getTutorURL = lookupFromMeta "pythontutor" . pageMeta

-- | construct a C tutor link with the student's submission and a test main
-- Note: this requires several convertions between various string types;
-- perhaps it could be made more efficient in the future?
makeTutorLink :: Submission -> Text -> Maybe Text
makeTutorLink Submission{..} url  = do
  guard (resultStatus submitResult == WrongAnswer)
  let code = codeText submitCode <> "\n" <> testMain (error "fix me")  -- resultReport submitResult)
  let params = Map.singleton "code" [B.fromString $ T.unpack code]
  return (T.pack $ B.toString $ queryURL (B.fromString $ T.unpack url) params)


testMain :: Text -> Text
testMain report
  = case getTestCase (T.unpack report) of
      Nothing ->  "/** couldn't construct a test main **/"
      Just TestMain{..} ->
        T.unlines [ "int main(void) {"
                  , T.pack testInit
                  , T.pack testCall <> ";"
                  , "/**"
                  , T.pack testMsg
                  , "**/"
                  , "}"
                  ]

-- | extract test main function from the report
data TestMain = TestMain { testInit :: String
                         , testCall :: String
                         , testMsg :: String
                         }
                deriving Show

getTestCase :: String -> Maybe TestMain
getTestCase str = do
  (_, _, str1, call:_) <- RE.matchRegexAll (RE.mkRegex "Testing (.*) with:") str
  (args, str2, str3, _) <- RE.matchRegexAll (RE.mkRegex "(Expected|Exception):") str1
  return TestMain { testCall = call, testInit = args, testMsg = str2 ++ str3 }




