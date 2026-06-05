{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

--
-- | Multiple-choice and fill-in quizzes
--
module Codex.Handlers.Quiz
  ( quizHandlers
  ) where

import           Codex.Types
import           Codex.Page
import           Codex.Application (Codex)
import qualified Codex.Application as App
import           Codex.Submission
import           Codex.Evaluate
import           Codex.Policy
import           Codex.Utils
import           Codex.Handlers
import           Codex.Quiz
import           Codex.Tester.Result

import           Snap.Core hiding (path)
import           Snap.Snaplet.Heist
import           Snap.Snaplet.Router

import           Heist
import qualified Heist.Splices as I
import qualified Heist.Interpreted as I

import qualified Text.Pandoc.Definition as P
import qualified Text.Pandoc.Builder as P

import           Data.Maybe (fromMaybe)
import qualified Data.Text           as T

import qualified Data.Map            as Map
import           Data.Map.Syntax
import           Data.List (intersperse, sort)
import           Data.Time.LocalTime

import           Control.Monad (guard)
import           Control.Monad.IO.Class (liftIO)


import qualified Data.Text.Encoding as T

decodeAnswers :: Text -> Maybe Answers
decodeAnswers txt = answers <$> decodeText txt

-- | get quiz answers from the HTTP parameters 
--
getFormAnswers :: MonadSnap m => Quiz -> m Answers
getFormAnswers Quiz{..}
  =  answersFromParams (map identifier questions)  <$> getParams

-- | get quiz answers from HTTP form parameters 
--
answersFromParams :: [Text] -> Params -> Answers
answersFromParams ids params =
  Answers $ Map.fromList [ (k', map T.decodeUtf8Lenient vs)
                             | (k,vs) <- Map.assocs params,
                               let k' = T.decodeUtf8Lenient k,
                               k' `elem` ids ]



-----------------------------------------------------------------
-- Heist splices
-----------------------------------------------------------------

quizSplices :: Monad m => Quiz -> Answers -> Splices (I.Splice m)
quizSplices Quiz{..} answers = do
  "quiz-preamble" ## return (blocksToHtml preamble)
  "questions" ##
    I.mapSplices (I.runChildrenWith . questionSplices answers) questions


questionSplices :: Monad m => Answers -> Question -> Splices (I.Splice m)
questionSplices answers question@Question{..} = do
  "question-name" ##  I.textSplice identifier
  "question-description" ## return (blocksToHtml description)
  let answer = lookupAnswer question answers
  choicesSplices options answer

choicesSplices :: Monad m => Options -> [Key] -> Splices (I.Splice m)
choicesSplices (Options selection attrs alts) selected = do
  let lalts = zip (listLabels attrs) alts
  let keys = sort [ label| (label, (True, _)) <- lalts ]
  let keyText = T.concat $ intersperse "," keys
  let answerText = T.concat $ intersperse "," selected
  "question-answer" ## I.textSplice answerText
  "question-answer-key" ## I.textSplice keyText
  "list-type" ## I.textSplice (listType attrs)
  "list-start" ## I.textSplice (listStart attrs)
  "onclick-callback" ## case selection of
                          Multiple -> return []
                          Single -> I.textSplice "onlyOne(this)"
  let laltSplices (label,(truth,item)) = do
        "alternative-label" ## I.textSplice label
        "alternative" ## return (blocksToHtml item)
        "if-checked" ## I.ifElseISplice (label `elem` selected)
        "if-correct" ## I.ifElseISplice truth
  "alternatives" ## I.mapSplices (I.runChildrenWith . laltSplices) lalts



listType :: P.ListAttributes -> Text
listType (_, style, _)
  = case style of
      P.LowerAlpha -> "a"
      P.UpperAlpha -> "A"
      P.LowerRoman -> "i"
      P.UpperRoman -> "I"
      _            -> "1"

listStart :: P.ListAttributes  -> Text
listStart (n, _, _) = T.pack (show n)

--
-- | handlers for viewing, submitting and reporting quizzes
--
quizView :: UserLogin -> FilePath -> Page -> Codex ()
quizView uid rqpath page = do
  guard (isQuiz page)
  let quiz = shuffleQuiz uid page
  subs <- getSubmissions uid rqpath
  -- fill-in last submitted answers
  let answers = fromMaybe mempty $ case subs of
                  [] -> Nothing
                  _ ->  getSubmitAnswers (last subs)
  otherSplices <- policySplices page uid rqpath
  renderWithSplices "_quiz" $ do
    quizSplices quiz answers
    otherSplices


getSubmitAnswers :: Submission -> Maybe Answers
getSubmitAnswers = decodeAnswers . codeText . submitCode

quizSubmit :: UserLogin -> FilePath -> Page -> Codex ()
quizSubmit uid rqpath page = do
  guard (isQuiz page)
  let quiz = shuffleQuiz uid page
  answers <- getFormAnswers quiz
  let txt = encodeText (QuizAnswers (quizToText quiz) answers)
  sid <- evaluateNew uid rqpath (Code "json" txt)
  redirectURL (App.Report sid)

isQuiz :: Page -> Bool
isQuiz page = pageTester page == Just "quiz"

-- | report a quiz submission
quizReport :: FilePath -> Page -> Submission -> Codex ()
quizReport rqpath page sub = do
  guard (isQuiz page)
  tz <- liftIO getCurrentTimeZone
  let quiz = shuffleQuiz (submitUser sub) page
  let answers = fromMaybe mempty $ getSubmitAnswers sub
  renderWithSplices "_answers" $ do
    urlSplices rqpath
    pageSplices page
    optionsSplices page
    submissionSplices tz sub
    summarySplice (submitResult sub)
    quizSplices quiz answers

summarySplice :: Result -> ISplices 
summarySplice Result{..} =
  "quiz-report-summary" ## return (blocksToHtml $ P.toList $ getBlocks resultReport) 



----------------------------------------------------------------------
-- | handler for generating printouts for quizzes
----------------------------------------------------------------------
quizPrintout :: UserLogin -> Page -> Submission -> Codex P.Blocks
quizPrintout _ page Submission{..}  = do
  guard (isQuiz page)
  return (content <> getBlocks report)
  where
    content = if checkPrintout page then
                ppQuiz quiz answers
              else
                mempty
    report = resultReport submitResult
    quiz = shuffleQuiz submitUser page
    answers = fromMaybe mempty $ decodeAnswers $ codeText submitCode


checkPrintout :: Page -> Bool
checkPrintout = fromMaybe True . lookupFromMeta "printout" . pageMeta


ppQuiz :: Quiz -> Answers -> P.Blocks
ppQuiz (Quiz _ questions) answers
  = mconcat (map (flip ppQuestion answers) questions)

ppQuestion :: Question -> Answers -> P.Blocks
ppQuestion question@Question{..} answers
  = P.fromList description <>
    ppOptions options (lookupAnswer question answers) <>
    ppKey options

ppKey :: Options -> P.Blocks
ppKey options
  = P.plain ("Correct answers: " <>
              mconcat (intersperse "," $ map P.text (answerKeys options)))

ppOptions :: Options -> [Key] -> P.Blocks
ppOptions (Options _ attrs alts) selected
  | correct = P.orderedListWith attrs (map (ppAlternative selected) lalts)
  | otherwise =  --- disabled question
      P.plain (P.emph "Disabled question") <>
      P.orderedListWith attrs (map (ppAlternative []) lalts)
  where
    correct = any fst alts      -- is there any correct answer?
    lalts = zip (listLabels attrs) alts


ppAlternative :: [Key] -> (Key, (Bool, [P.Block])) -> P.Blocks
ppAlternative selected (label, (truth, blocks))
  = label1 <> P.plain P.space <> P.fromList blocks <> P.plain P.space <> label2
  where
    reply = label `elem` selected
    label1 = P.plain $ P.math $ if reply then "☒" else "☐"
    label2 | reply && truth = P.plain $ P.strong $ P.text "✅"
           | reply && not truth = P.plain $ P.strong $ P.text "❌"
           | not reply && truth = P.plain $ P.strong $ P.text ""
           | otherwise = mempty

    
-- | record with quiz handlers
quizHandlers :: Handlers Codex
quizHandlers
  = Handlers quizView quizSubmit (const quizReport) quizPrintout



