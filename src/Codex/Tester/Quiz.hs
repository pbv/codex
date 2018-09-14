--
-- | Accept and score a multiple choice quiz
--
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Codex.Tester.Quiz (
  quizTester
  ) where


import           Codex.Tester
import           Codex.Quiz
import           Data.Text(Text)
import qualified Data.Text as T

import qualified Data.Aeson as Aeson
import           Data.Maybe
import           Data.Monoid
import           Data.Ratio
import           Data.List (intersect, (\\))

import           Text.Printf(printf)

quizTester :: Tester Result
quizTester = tester "quiz" $ do
  Code lang text <- testCode
  guard (lang == "json")
  page <- testPage
  let quiz = makeQuiz page
  let answers = fromMaybe emptyAnswers (decodeAnswers text)
  let Score{..} = scoreAnswers quiz answers
  let percent = realToFrac (100 * accum /
                            fromIntegral (numQuestions quiz)) :: Double
  return $
    accepted $
    T.unlines [ T.pack (printf "%d correct and %d incorrect answers."
                        correct wrong)
              , T.pack (printf "Score: %.2f%%" percent)
              ]

numQuestions :: Quiz -> Int
numQuestions (Quiz _ questions) = length questions


-- | a record for grading quizzes
data Score = Score { correct :: !Int
                   , wrong   :: !Int
                   , accum :: !(Ratio Int) -- ^ cumulative percentages
                   }
             deriving Show

instance Monoid Score where
  mempty = Score 0 0 0 
  s1 `mappend` s2
    = Score { correct = correct s1 + correct s2
            , wrong   = wrong s1 + wrong s2
            , accum   = accum s1 + accum s2
            }

scoreAnswers :: Quiz -> Answers -> Score
scoreAnswers (Quiz _ questions) answers
  = mconcat (map (flip scoreQuestion answers) questions)

scoreQuestion :: Question -> Answers  -> Score
scoreQuestion question@(Question _ attr alts) answers
  | num_correct>0 && num_wrong>0 = Score correct wrong grade
  | otherwise                    = mempty   -- invalid question
  where selected = lookupAnswers question answers
        key = [ label | (label,True,_) <- alts ]
        num_alts = length alts
        num_correct = length key
        num_wrong = num_alts - num_correct
        correct = length (selected `intersect` key)
        wrong = length (selected \\ key)
        grade = (correct % num_correct) - (wrong % num_wrong)
  
