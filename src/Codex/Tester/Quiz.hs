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
import           Codex.Utils (decodeText)
import           Codex.Quiz hiding (questions)
import           Data.Text (Text)
import qualified Data.Text as T

import           Data.Maybe
import           Data.Ratio
import           Data.List (intersect, (\\))

import           Data.Char (isSpace)
import           Text.Printf (printf)
import qualified Text.Pandoc.Builder as P

type Fraction = Ratio Int

-- | grading score parameters
data Scores
  = Scores { correctScore :: Maybe Fraction
           , incorrectScore :: Maybe Fraction
           }
    deriving Show

-- | grading a Quiz
-- invariant:  numOptions == numCorrect + numIncorrect
data Grades
  = Grades { numOptions :: !Int    -- ^ total number of options
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
  -- optional grading scores
  scores <- Scores <$> metadata "correct-score"
                   <*> metadata "incorrect-score"
  let quiz = shuffleQuiz uid page
  let selected = maybe mempty answers (decodeText text)
  let grades = gradeQuiz scores quiz selected
  let summary = makeSummary scores grades
  return $ accepted summary


makeSummary :: Scores -> Grades -> P.Blocks
makeSummary Scores{..} Grades{..}
  = P.bulletList $ map P.plain  
    [ P.text "Number of questions: " <> P.str (tshow numQuestions)
    , P.text "Total number of options: " <> P.str (tshow numOptions)
    , P.text "Number of correct choices: " <> P.str (tshow numCorrect) 
    , P.text "Number of incorrect choices: " <> P.str (tshow numIncorrect)
    , P.text "Score for correct choices: " <> P.str (showFraction' correctScore)
    , P.text "Score for incorrect choices: " <> P.str (showFraction' incorrectScore)
    , P.text "Grade: " <> P.str (T.pack $ printf "%.2f%%" (100*grade)) 
    ]
  where
    tshow :: Show a => a -> Text
    tshow = T.pack . show
    showFraction' :: (Integral a, Show a) => Maybe (Ratio a) -> Text
    showFraction' = maybe "default" showFraction
    numQuestions = length gradesList
    grade = if numQuestions > 0
            then fromFraction (sum gradesList / fromIntegral numQuestions)
            else 0


fromFraction :: Fraction -> Float
fromFraction f = fromIntegral (numerator f) / fromIntegral (denominator f)


showFraction :: (Integral a, Show a) => Ratio a -> Text
showFraction r
  = T.pack (show (numerator r)) <> 
    if denominator r /= 1 then
      "/" <> T.pack (show (denominator r))
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


-- | grade all questions in a quiz
--
gradeQuiz :: Scores -> Quiz -> Answers -> Grades
gradeQuiz weights (Quiz _ questions) answers
  = mconcat [gradeQuestion weights q answers | q<-questions]

-- | grade a single question
--
gradeQuestion :: Scores -> Question -> Answers -> Grades
gradeQuestion weights question answers
  = gradeChoices weights (choices question) (lookupAnswer question answers)

gradeChoices :: Scores -> Choices -> [Key] -> Grades
gradeChoices Scores{..} (Alternatives _ attrs alts) selected
  | num_correct>0 && num_wrong>0
  = Grades num_keys correct wrong [grade]
  | otherwise
  = mempty           -- invalid question; no score
  where key = [ label | (label,(True,_)) <- zip (listLabels attrs) alts ]
        num_keys = length key
        num_alts = length alts
        num_correct = num_keys -- length key
        num_wrong = num_alts - num_correct
        correct = length (selected `intersect` key)
        wrong = length (selected \\ key)
        -- positive grade for correctly answered alternatives
        positive = case correctScore of
          Nothing -> correct%num_correct
          Just w -> w * fromIntegral correct
        -- negative grade for incorrectly answered alternatives    
        negative = case incorrectScore of
          Nothing -> - (wrong%num_wrong)
          Just w -> w * fromIntegral wrong
        -- combined grade for this question
        grade = (-1) `max` (positive + negative) `min` 1


gradeChoices _ (FillIn keyText normalize) answers
  = Grades 1 correct wrong [grade]
  where grade = fromIntegral correct
        correct = if normalize answerText == normalize keyText
                  then 1 else 0
        wrong = 1 - correct
        answerText = T.concat answers


