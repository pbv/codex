--
-- | Accept and score a multiple choice quiz
--
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Codex.Tester.Quiz (
  quizTester
  ) where


import           Codex.Tester
import           Codex.Handlers.Quiz
-- import           Data.Text(Text)
import qualified Data.Text as T

--import qualified Data.Aeson as Aeson
import           Data.Maybe
--import           Data.Monoid
import           Data.Ratio
import           Data.List (intersect, (\\))

import           Text.Printf(printf)

quizTester :: Tester Result
quizTester = tester "quiz" $ do
  Code lang text <- askSubmitted
  guard (lang == "json")
  page <- askPage
  let quiz = makeQuiz page
  let answers = fromMaybe emptyAnswers (decodeAnswers text)
  let Score{..} = scoreQuiz quiz answers
  let percent = realToFrac (100 * accum / fromIntegral total) :: Double
  return $
    accepted $
    T.unlines $ map T.pack
       [ printf "Answered %d of %d questions." answered total
       , printf "%d correct and %d incorrect replies." correct wrong
       , printf "Score: %.2f%%" percent
       ]



-- | a record for grading quizzes
data Score
  = Score { total   :: !Int       -- ^ total # of questions
          , answered :: !Int      -- ^ number of answered questions
          , correct :: !Int       -- ^ correct answer counter
          , wrong   :: !Int       -- ^ wrong answer counter
          , accum   :: !(Ratio Int) -- ^ cumulative percentages
          }
  deriving Show

instance Semigroup Score where
  s1 <> s2
    = Score { total   = total s1 + total s2
            , answered = answered s1 + answered s2
            , correct = correct s1 + correct s2
            , wrong   = wrong s1 + wrong s2
            , accum   = accum s1 + accum s2
            }

instance Monoid Score where
  mempty = Score 0 0 0 0 0

-- | score all questions in a quiz
--
scoreQuiz :: Quiz -> Answers -> Score
scoreQuiz (Quiz _ questions) answers
  = mconcat (map (flip scoreQuestion answers) questions)

-- | score a single question
--
scoreQuestion :: Question -> Answers  -> Score
scoreQuestion question@(Question _ attr alts) answers
  | num_correct>0 && num_wrong>0 = Score 1 answered correct wrong grade
  | otherwise                    = Score 1 0 0 0 0
                                    -- invalid question
  where selected = lookupAnswers question answers
        answered = if null selected then 0 else 1
        key = [ label | (label,True,_) <- alts ]
        num_alts = length alts
        num_correct = length key
        num_wrong = num_alts - num_correct
        correct = length (selected `intersect` key)
        wrong = length (selected \\ key)
        grade = (correct % num_correct) - (wrong % num_wrong)
  
