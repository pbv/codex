{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

--
-- Code exercise handlers
--
module Codex.Handlers.Code
  ( codeHandlers
  ) where

import          Codex.Types
import          Codex.Application
import          Codex.Utils
import          Codex.Handlers
import          Codex.Page
import          Codex.Submission
import          Codex.AceEditor
import          Codex.Evaluate
import          Codex.Policy
import          Codex.Tester.Result
import          Codex.Pythontutor
  
import           Snap.Snaplet.Heist
import           Snap.Snaplet.Router

import qualified Heist.Interpreted                           as I

import qualified Text.Pandoc.Builder as P

import           Data.Maybe (isJust)
import           Data.Map.Syntax
import qualified Data.Text                                   as T
import           Data.Time.LocalTime

import           Control.Monad (guard)
import           Control.Monad.IO.Class (liftIO)

-- | check for an exercise page 
isExercise :: Page -> Bool
isExercise = isJust . pageTester

-- | get a coding exercise 
codeView :: UserLogin -> FilePath -> Page -> Codex ()
codeView uid rqpath page = do
  guard (isExercise page)
  tz <- liftIO getCurrentTimeZone
  subs <- getSubmissions uid rqpath
  otherSplices <- policySplices page uid rqpath
  renderWithSplices "_exercise" $ do
    pageSplices page
    codeSplices page
    optionsSplices page
    submissionListSplices tz subs
    textEditorSplice
    languageSplices (pageLanguages page) Nothing
    otherSplices


codeSubmit :: UserLogin -> FilePath -> Page -> Codex ()
codeSubmit uid rqpath page = do
  guard (isExercise page)
  text <- require (getTextParam "code")
  lang <- Language <$> require (getTextParam "language")
  guard (lang `elem` pageLanguages page) 
  sid <- evaluateNew uid rqpath (Code lang text)
  redirectURL (Report sid)

-- | report a code submission
codeReport :: FilePath -> Page -> Submission -> Codex ()
codeReport rqpath page sub@Submission{..} = do
  guard (isExercise page)
  tz <- liftIO getCurrentTimeZone
  otherSplices <- policySplices page submitUser submitPath
  renderWithSplices "_report" $ do
    urlSplices rqpath
    pageSplices page
    optionsSplices page
    codeSplices page
    submissionSplices tz sub
    pythontutorSplices page sub
    textEditorSplice
    languageSplices (pageLanguages page) (Just $ submitLang sub)
    otherSplices


-- | splices related to code exercises
codeSplices :: Page -> ISplices
codeSplices page = do
  "page-languages" ##
    I.textSplice $ T.intercalate "," $ map fromLanguage $ pageLanguages page
  "language-extensions" ##
   I.textSplice $ languageExtensions $ pageLanguages page
  "default-text" ## maybe (return []) I.textSplice (pageDefaultText page)


-- | splices relating to a list of submissions
submissionListSplices :: TimeZone -> [Submission] -> ISplices
submissionListSplices tz list = do
  "submissions-count" ## I.textSplice (T.pack $ show $ length list)
  "submissions-list" ##
    I.mapSplices (I.runChildrenWith . submissionSplices tz) list   


codePrintout :: Page -> Submission -> Codex P.Blocks
codePrintout page Submission{..} = do
  guard (isExercise page)
  let lang = T.unpack $ fromLanguage $ codeLang submitCode
  let code = T.unpack $ codeText submitCode
  let report = T.unpack $ resultReport submitResult
  return (P.codeBlockWith ("", [lang, "numberLines"], []) code <>
          P.codeBlock report)




codeHandlers :: Handlers Codex
codeHandlers
  = Handlers
    { handleView = codeView
    , handleSubmit = codeSubmit
    , handleReport = const codeReport
    , handlePrintout = const codePrintout
    }
