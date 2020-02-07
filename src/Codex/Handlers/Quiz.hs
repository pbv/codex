{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}

--
-- | Multiple-choice and fill-in quizzes
--
module Codex.Handlers.Quiz
  ( quizHandlers
  ) where

import           Codex.Types
import           Codex.Page
import           Codex.Application
import           Codex.Submission
import           Codex.Evaluate
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

import           Data.Ratio
import           Data.Maybe (fromMaybe)
import qualified Data.Text           as T
import           Data.Text(Text)
import qualified Data.ByteString.UTF8 as B

import qualified Data.HashMap.Strict as HashMap
import qualified Data.Map            as Map
import qualified Data.Aeson          as Aeson
import           Data.Aeson.Types    as Aeson
import           Data.Map.Syntax
import           Data.List (intersperse, sort)
import           Data.Time.LocalTime

import           Control.Monad (guard, join)
import           Control.Monad.IO.Class (liftIO)

import           Text.Printf

decodeAnswers :: Text -> Maybe Answers
decodeAnswers txt = answers <$> decodeText txt

-- | get quiz answers from the HTTP parameters 
--
getFormAnswers :: MonadSnap m => Quiz -> m Answers
getFormAnswers Quiz{..}
  = (answersFromParams . filterParams) <$> getParams
  where
    filterParams = Map.filterWithKey (\k _ -> k `elem` idents)
    idents = map (B.fromString . identifier) questions

-- | get quiz answers from HTTP form parameters 
--
answersFromParams :: Params -> Answers
answersFromParams params =
  Answers $ HashMap.fromList [ (B.toString k, map B.toString vs)
                             | (k,vs) <- Map.assocs params ]



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
  "question-name" ##  I.textSplice (T.pack identifier)
  "question-description" ## return (blocksToHtml description)
  let answer = lookupAnswer question answers
  choicesSplices choices answer

choicesSplices :: Monad m => Choices -> [Key] -> Splices (I.Splice m)
choicesSplices (FillIn keyText normalize) answers = do
  let answerText = T.concat $ map T.pack answers
  "question-answer" ## I.textSplice answerText
  "question-answer-key" ##  I.textSplice keyText
  "if-correct" ## I.ifElseISplice (normalize answerText ==
                                   normalize keyText)
  "question-fillin" ## I.ifElseISplice True

choicesSplices (Alternatives selection attrs alts) selected = do
  let lalts = zip (listLabels attrs) alts
  let keys = sort [ label| (label, (True, _)) <- lalts ]
  let keyText = T.concat $ intersperse "," $ map T.pack keys
  let answerText = T.concat $ intersperse "," $ map T.pack selected
  "question-answer" ## I.textSplice answerText
  "question-answer-key" ## I.textSplice keyText
  "list-type" ## I.textSplice (listType attrs)
  "list-start" ## I.textSplice (listStart attrs)
  "onclick-callback" ## case selection of
                          Multiple -> return []
                          Single -> I.textSplice "onlyOne(this)"
  let laltSplices (label,(truth,item)) = do
        "alternative-label" ## I.textSplice (T.pack label)
        "alternative" ## return (blocksToHtml item)
        "if-checked" ## I.ifElseISplice (label `elem` selected)
        "if-correct" ## I.ifElseISplice truth
  "alternatives" ## I.mapSplices (I.runChildrenWith . laltSplices) lalts
  "question-fillin" ## I.ifElseISplice False



listType :: P.ListAttributes -> Text
listType (_, style, _)
  = case style of
      P.LowerAlpha -> "a"
      P.UpperAlpha -> "A"
      P.LowerRoman -> "i"
      P.UpperRoman -> "I"
      _          -> "1"

listStart :: P.ListAttributes  -> Text
listStart (n, _, _) = T.pack (show n)

--
-- | handlers for viewing, submitting and reporting quizzes
--
quizView :: UserLogin -> FilePath -> Page -> Codex ()
quizView uid rqpath page = do
  guard (isQuiz page)
  let quiz = shuffleQuiz uid page
  subs <- getPageSubmissions uid rqpath
  -- fill-in last submitted answers
  let answers = fromMaybe mempty $ case subs of
                  [] -> Nothing
                  _ ->  getSubmitAnswers (last subs)
  withPolicySplices uid rqpath page $
    renderWithSplices "_quiz" $ quizSplices quiz answers


getSubmitAnswers :: Submission -> Maybe Answers
getSubmitAnswers = decodeAnswers . codeText . submitCode 

quizSubmit :: UserLogin -> FilePath -> Page -> Codex ()
quizSubmit uid rqpath page = do
  guard (isQuiz page)
  let quiz = shuffleQuiz uid page
  answers <- getFormAnswers quiz
  let txt = encodeText (QuizAnswers (quizToText quiz) answers)
  sid <- newSubmission uid rqpath (Code "json" txt)
  redirectURL (Report sid)

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
    feedbackSplices page
    submitSplices tz sub
    summarySplice (submitResult sub)
    quizSplices quiz answers


summarySplice Result{..} = 
  "quiz-report-summary" ##
    maybe (return []) I.textSplice (reportSummary <$> decodeText resultReport)


verbosePrintout :: Page -> Bool
verbosePrintout = fromMaybe True . lookupFromMeta "printout" . pageMeta 

-------------------------------------------------------------------
-- Summary
-------------------------------------------------------------------

type Fraction  = Ratio Int
                  
-- report the summary in human-readable text
reportSummary :: Map.Map String Aeson.Value -> Text
reportSummary summary
  = T.unlines $ map T.pack
    [ maybe "" (printf "Number of questions: %d") num_questions
    , maybe "" (printf "Total number of options: %d") num_options
    , maybe "" (printf "Number of correct choices: %d") num_correct
    , maybe "" (printf "Number of incorrect choices: %d") num_incorrect
    , "Weight for correct choices: " <>
      maybe "default" showFraction correct_weight
    , "Weight for incorrect choices: " <>
      maybe "default" showFraction incorrect_weight
    , maybe "" (printf "Score: %.2f%%") score_percent
    ]
  where
    fetch :: FromJSON v => String -> Maybe v
    fetch name = Map.lookup name summary >>=
                 Aeson.parseMaybe Aeson.parseJSON 

    num_questions = fetch @Int "number_of_questions"
    num_options   = fetch @Int "number_of_options"
    num_correct   = fetch @Int "correct_selections"
    num_incorrect = fetch @Int "incorrect_selections"
    
    correct_weight
      = join (fetch @(Maybe Fraction) "correct_weight_ratio")
    incorrect_weight
      = join (fetch @(Maybe Fraction) "incorrect_weight_ratio")
    score_percent = (100*) <$> fetch @Double "score_percent"

   
showFraction :: (Integral a, Show a) => Ratio a -> String
showFraction r
  = show (numerator r) ++
    if (denominator r /= 1) then
      "/" ++ show (denominator r)
    else ""



----------------------------------------------------------------------
-- | handler for generating printouts for quizzes
----------------------------------------------------------------------
quizPrintout :: UserLogin -> Page -> Submission -> Codex P.Blocks
quizPrintout _ page Submission{..}  = do
  guard (isQuiz page)
  return (content <> P.codeBlock report)
  where
    content = if verbosePrintout page then ppQuiz quiz answers
              else mempty
    report = maybe "" T.unpack $
              reportSummary <$> (decodeText $ resultReport submitResult)
    quiz = shuffleQuiz submitUser page
    answers = fromMaybe mempty $ decodeAnswers $ codeText $ submitCode

ppQuiz :: Quiz -> Answers -> P.Blocks
ppQuiz (Quiz _ questions) answers
  = mconcat (map (flip ppQuestion answers) questions)

ppQuestion :: Question -> Answers -> P.Blocks
ppQuestion question@Question{..} answers
  = P.fromList description <>
    ppChoices choices (lookupAnswer question answers) 

  
ppChoices :: Choices -> [Key] -> P.Blocks
ppChoices (FillIn keyText _) answers
  = P.plain (mconcat $ map P.text answers) <>
    P.plain (P.emph "Answer:" <> P.text (T.unpack keyText))

ppChoices (Alternatives _ attrs alts) selected
  = P.orderedListWith attrs (map (ppAlternative selected) lalts)
  where lalts = zip (listLabels attrs) alts

ppAlternative :: [Key] -> (Key, (Bool, [P.Block])) -> P.Blocks
ppAlternative selected (label, (truth, blocks))
  = label1 <> P.fromList blocks <> label2
  where
    reply = label `elem` selected
    label1 = P.plain $ P.math $ if reply then "\\bullet" else "\\circ"
    label2 | reply && truth = P.plain $ P.strong $ P.text "OK"
           | reply && not truth = P.plain $ P.strong $ P.text "WRONG"
           | not reply && truth = P.plain $ P.strong $ P.text "MISS"
           | otherwise = mempty
           
    
-- | record with quiz handlers
quizHandlers :: Handlers Codex
quizHandlers
  = Handlers quizView quizSubmit (const quizReport) quizPrintout

 
