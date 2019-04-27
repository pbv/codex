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
import           Codex.Page 
import           Codex.Handlers.Quiz
import qualified Data.Text as T

import           Data.Maybe
import           Data.Ratio
import           Data.List (intersect, (\\))

import           Data.Char (isSpace)
import           Text.Printf(printf)

type Fraction = Ratio Int

-- | grading weights
data Weights
  = Weights { rightWeight :: Maybe Fraction
            , wrongWeight :: Maybe Fraction
            }
    deriving Show


-- | a record for grading quizzes
data Score
  = Score { total   :: !Int       -- ^ total number of questions
          , answered :: !Int      -- ^ number of answered questions
          , correct :: !Int       -- ^ number of correct answers
          , wrong   :: !Int       -- ^ number of wrong answers
          , accum   :: !Fraction  -- ^ accumulated percentages
          }
  deriving Show

-- | joining scores
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

quizTester :: Tester Result
quizTester = tester "quiz" $ do
  Code lang text <- askSubmitted
  guard (lang == "json")
  page <- askPage
  -- optional grade weights
  weights@Weights{..} <-  Weights <$>
                          metadata "correct-weight" <*>
                          metadata "incorrect-weight"
  let quiz = makeQuiz page
  let answers = fromMaybe emptyAnswers (decodeAnswers text)
  let Score{..} = scoreQuiz weights quiz answers
  let percent = realToFrac (100 * accum / fromIntegral total) :: Double
  return $
    accepted $
    T.unlines $ map T.pack
       [ printf "Answered %d of %d questions." answered total
       , printf "%d correct and %d incorrect replies." correct wrong
       , "Weight for correct answers: " ++ showWeight rightWeight
       , "Weight for incorrect answers: " ++ showWeight wrongWeight
       , printf "Score: %.2f%%" percent
       ]
  where
    showWeight :: Maybe Fraction -> String
    showWeight Nothing = "default"
    showWeight (Just w) = showFrac w

showFrac :: (Integral a, Show a) => Ratio a -> String
showFrac r = show (numerator r) ++
             if (denominator r /= 1) then
               "/" ++ show (denominator r)
             else ""

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
scoreQuiz :: Weights -> Quiz -> Answers -> Score
scoreQuiz weights (Quiz _ questions) answers
  = mconcat (map (scoreQuestion weights answers) questions)

-- | score a single question
--
scoreQuestion :: Weights -> Answers -> Question -> Score
scoreQuestion Weights{..} answers question@(Question _ _ alts) 
  | num_correct>0 && num_wrong>0
  = Score 1 answered correct wrong grade 
  | otherwise
  = mempty           -- no scoring for invalid questions
  where selected = lookupAnswers question answers
        answered = if null selected then 0 else 1
        key = [ label | (label,True,_) <- alts ]
        num_alts = length alts
        num_correct = length key
        num_wrong = num_alts - num_correct
        correct = length (selected `intersect` key)
        wrong = length (selected \\ key)
        -- grade = (correct % num_correct) - (wrong % num_wrong)
        positive = maybe (correct%num_correct) (*fromIntegral correct)
                   rightWeight
        negative = maybe (-wrong%num_wrong) (*fromIntegral wrong)
                   wrongWeight
        grade = (-1) `max` (positive + negative) `min` 1
                
    
