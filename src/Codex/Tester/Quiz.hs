--
-- | Accept and score a multiple choice quiz
--
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Codex.Tester.Quiz (
  quizTester
  ) where


import           Codex.Tester
import           Codex.Page 
import           Codex.Utils (decodeText, encodeText)
import           Codex.Quiz hiding (questions)
import qualified Data.Text as T

import           Data.Maybe
import           Data.Ratio
import           Data.List (intersect, (\\))
import qualified Data.Map as Map
import           Data.Map(Map)

import           Data.Char (isSpace)

import           Data.Aeson (toJSON, Value)

type Fraction = Ratio Int

-- | grading parameters
data Weights
  = Weights { correctWeight :: Maybe Fraction
            , incorrectWeight :: Maybe Fraction
            }
    deriving Show

-- | scoring a Quiz
-- invariant:  numOptions == numCorrect + numIncorrect
data Scores
  = Scores { numOptions :: !Int    -- ^ total number of options
           , numCorrect :: !Int    -- ^ number of correct selections
           , numIncorrect :: !Int  -- ^ number of incorrect selections
           , scoresList :: [Fraction] -- ^ scores for questions
           }
  deriving (Show)

-- | joining scorings
instance Semigroup Scores where
  s <> s'
    = Scores { numOptions = numOptions s + numOptions s'
             , numCorrect = numCorrect s + numCorrect s'
             , numIncorrect = numIncorrect s + numIncorrect s'
             , scoresList = scoresList s ++ scoresList s'
             }

instance Monoid Scores where
  mempty = Scores 0 0 0 []


quizTester :: Tester Result
quizTester = tester "quiz" $ do
  Code lang text <- testCode
  guard (lang == "json")
  page <- testPage
  uid <- testUser
  -- optional grading weights
  weights <- Weights <$> metadata "correct-weight" <*>
                         metadata "incorrect-weight"
  let quiz = shuffleQuiz uid page
  let selected = fromMaybe mempty (answers <$> decodeText text)
  let scores = scoreQuiz weights quiz selected
  let summary = makeSummary weights scores
  return $ accepted $ encodeText summary


makeSummary :: Weights -> Scores -> Map String Value
makeSummary Weights{..} Scores{..}
  = Map.fromList $
    [ ("number_of_questions", toJSON numQuestions)
    , ("number_of_options", toJSON numOptions)
    , ("correct_selections", toJSON numCorrect)
    , ("incorrect_selections", toJSON numIncorrect)
    , ("correct_weight_ratio", toJSON correctWeight)
    , ("correct_weight_percent", toJSON (percent <$> correctWeight))
    , ("incorrect_weight_ratio", toJSON incorrectWeight)
    , ("incorrect_weight_percent", toJSON (percent <$> incorrectWeight))
    , ("score_ratio", toJSON score)
    , ("score_percent", toJSON (percent score))
    ]
  where
    percent :: Fraction -> Double
    percent = realToFrac
    numQuestions = length scoresList
    score = if numQuestions > 0
            then sum scoresList / fromIntegral numQuestions
            else 0
        


instance (Integral a, Read a) => FromMetaValue (Ratio a) where
  fromMeta v = fromMeta v >>= readFrac

readFrac :: (Integral a, Read a) => String -> Maybe (Ratio a)
readFrac r =
  listToMaybe
  ([ num % denom
   | (num,s) <- reads r,
     ("/",t) <- lex s,
    (denom,u)   <- reads t
   , all isSpace u ]
   ++
   [ x % 1
   | (x, u) <- reads r
   , all isSpace u ]
   ++
   [ num % denom
   | (x :: Double, u) <- reads r
   , all isSpace u
   , let f = approxRational x 1e-3
   , let num = fromInteger (numerator f)
   , let denom = fromInteger (denominator f)
   ]
  )

    
-- | score all questions in a quiz
--
scoreQuiz :: Weights -> Quiz -> Answers -> Scores 
scoreQuiz weights (Quiz _ questions) answers
  = mconcat [scoreQuestion weights q answers | q<-questions]

-- | score a single question
--
scoreQuestion :: Weights -> Question -> Answers -> Scores 
scoreQuestion weights question answers 
  = scoreChoices weights (choices question) (lookupAnswer question answers)

scoreChoices :: Weights -> Choices -> [Key] -> Scores 
scoreChoices Weights{..} (Alternatives _ attrs alts) selected
  | num_correct>0 && num_wrong>0
  = Scores num_keys correct wrong [grade]
  | otherwise
  = mempty           -- invalid question; no score
  where key = [ label | (label,(True,_)) <- zip (listLabels attrs) alts ]
        num_keys = length key
        num_alts = length alts
        num_correct = length key
        num_wrong = num_alts - num_correct
        correct = length (selected `intersect` key)
        wrong = length (selected \\ key)
        -- positive grade for correctly answered alternatives
        positive = case correctWeight of
          Nothing -> correct%num_correct
          Just w -> w * fromIntegral correct
        -- negative grade for incorrectly answered alternatives    
        negative = case incorrectWeight of
          Nothing -> -wrong%num_wrong
          Just w -> w * fromIntegral wrong
        -- combined grade for this question
        grade = (-1) `max` (positive + negative) `min` 1
          

scoreChoices _ (FillIn keyText normalize) answers
  = Scores 1 correct wrong [grade]
  where grade = fromIntegral correct 
        correct = if normalize answerText == normalize keyText
                  then 1 else 0
        wrong = 1 - correct
        answerText = T.concat $ map T.pack answers 
        
        
