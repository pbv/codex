{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Codex.Pythontutor where

import           Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Text.Regex as T

import           Heist.Splices     as I
import qualified Heist.Interpreted as I

import           Data.Map.Syntax

import           Codex.Tester.Result
import           Codex.Submission.Types
import           Codex.Page
import           Codex.Types
import           Codex.Utils


pythontutorSplices :: Page -> Submission -> ISplices
pythontutorSplices page sub@Submission{..} = do
  "if-pythontutor" ## I.ifElseISplice (usePythonTutor page submitResult)
  "pythontutor-url" ## if (usePythonTutor page submitResult) 
                       then I.textSplice (linkEncode (fromJust (pageTutor page)) (genTutorCode sub)) 
                       else I.textSplice (T.empty)

usePythonTutor :: Page -> Result -> Bool
usePythonTutor page Result{..} = (isJust $ pageTutor page) && (resultStatus == WrongAnswer)

pageTutor :: Page -> Maybe T.Text
pageTutor = lookupFromMeta "pythontutor" . pageMeta

genTutorCode :: Submission -> T.Text
genTutorCode Submission{..} = (codeText submitCode) <> (formatResult $ resultReport submitResult)

linkEncode :: T.Text -> T.Text -> T.Text
linkEncode url code = T.decodeUtf8 $ ctutor (T.encodeUtf8 url) $ T.encodeUtf8 code

matchText :: String -> T.Text -> Bool
matchText regex t = isJust $ T.matchRegex (T.mkRegex regex) $ T.unpack t

formatResult :: T.Text -> T.Text
formatResult t = getCMain $ getResArgs $ dropWhile (not . matchText "Testing") tl
 where tl = T.lines t

getResArgs :: [T.Text] -> [T.Text]
getResArgs tl = (takeWhile (not . matchText "(Expected|Exception)") (tail tl)) ++ [getResFunc (tl!!0)]

getResFunc :: T.Text -> T.Text
getResFunc t = (T.dropWhileEnd (/=')') $ T.dropWhile (/=' ') t) <> ";"

getCMain :: [T.Text] -> T.Text
getCMain args = T.unlines $ (["int main() {"] ++ args ++ ["}"])
