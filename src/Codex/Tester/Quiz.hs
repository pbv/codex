--
-- | Accept and score a multiple choice quiz
--
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Codex.Tester.Quiz (
  quizTester
  ) where


import           Codex.Tester
import           Codex.Utils (decodeText)
import           Codex.Quiz hiding (questions)
import           Data.Text (Text)
import qualified Data.Text as T

import           Data.Ratio
import           Data.List (intersect, (\\))

import           Text.Printf (printf)
import qualified Text.Pandoc.Builder as P

type Fraction = Ratio Int

-- | grading a Quiz
-- invariant:  numOptions == numCorrect + numIncorrect
data Grades
  = Grades { numOptions :: !Int    -- ^ total number of options selected
           , numCorrect :: !Int    -- ^ number of correct selections
           , numIncorrect :: !Int  -- ^ number of incorrect selections
           , gradesList :: [Fraction] -- ^ grades for each question
           }
  deriving (Show)

-- | joining grades 
instance Semigroup Grades where
  s <> s'
    = Grades { numOptions = numOptions s + numOptions s'
             , numCorrect = numCorrect s + numCorrect s'
             , numIncorrect = numIncorrect s + numIncorrect s'
             , gradesList = gradesList s ++ gradesList s'
             }

instance Monoid Grades where
  mempty = Grades 0 0 0 []


quizTester :: Tester Result
quizTester = tester "quiz" $ do
  Code lang text <- testCode
  guard (lang == "json")
  page <- testPage
  uid <- testUser
  offset <- metadataWithDefault "grade-offset" 0.0
  let quiz = shuffleQuiz uid page
  let selected = maybe mempty answers (decodeText text)
  let grades = gradeQuiz quiz selected
  let summary = makeSummary offset grades
  return $ accepted summary


makeSummary :: Double -> Grades -> P.Blocks
makeSummary offset Grades{..}
  = P.bulletList $ map P.plain  
    [ P.text "Number of questions: " <> P.str (tshow numQuestions)
    , P.text "Total number of options: " <> P.str (tshow numOptions)
    , P.text "Number of correct choices: " <> P.str (tshow numCorrect) 
    , P.text "Number of incorrect choices: " <> P.str (tshow numIncorrect)
    , P.text "Total score: " <>
      P.str (T.pack $ printf "%.2f%%" (100*total))
    , P.text "Grade: " <>
      P.str (T.pack $ printf "%.2f%%" (100*grade)) 
    ]
  where
    tshow :: Show a => a -> Text
    tshow = T.pack . show
    numQuestions = length gradesList
    total, grade :: Double
    total = if numQuestions > 0
            then fromFraction (sum gradesList / fromIntegral numQuestions)
            else 0
    grade = max 0 ((total-offset)/(1-offset))

fromFraction :: Fraction -> Double
fromFraction f = fromIntegral (numerator f) / fromIntegral (denominator f)


-- | grade all questions in a quiz
--
gradeQuiz :: Quiz -> Answers -> Grades
gradeQuiz (Quiz _ questions) answers
  = mconcat [gradeQuestion q answers | q<-questions]

-- | grade a single question
--
gradeQuestion :: Question -> Answers -> Grades
gradeQuestion question answers
  = gradeOptions (options question) (lookupAnswer question answers)

gradeOptions :: Options -> [Key] -> Grades
gradeOptions (Options kind attrs alts) selected
  | numCorrect>0 && numIncorrect>0
  = Grades numSelected choseCorrect choseIncorrect [score]
  | otherwise = mempty           -- invalid question; no grading
  where keys = [label | (label,(True,_)) <- zip (listLabels attrs) alts]
        numAlts = length alts
        numCorrect = length keys
        numIncorrect = numAlts - numCorrect
        numSelected = length selected
        choseCorrect = length (selected `intersect` keys)
        choseIncorrect = length (selected \\ keys)
        score = case kind of
          Single ->
            if choseCorrect>0 then 1 else 0
          Multiple ->
            max 0 ((choseCorrect - choseIncorrect) % numCorrect)




