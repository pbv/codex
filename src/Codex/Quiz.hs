{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}

-- | Quizzes with multiple choice & fill-in questions
--
module Codex.Quiz
  ( Quiz(..)
  , Question(..)
  , Choices(..)
  , Selection(..)
  , Answers(..)
  , Key
  , QuizAnswers(..)
  , shuffleQuiz
  , lookupAnswer
  , listLabels
  , quizToDocument
  , quizToText
  ) where

import           Codex.Types
import           Codex.Page
import           Codex.Random (Rand)
import qualified Codex.Random as Rand

import qualified Text.Pandoc.Builder as P
import qualified Text.Pandoc.Walk as P
import qualified Text.Pandoc.Writers.Markdown as P
import qualified Text.Pandoc.Class as P
import qualified Text.Pandoc.Options as P


import qualified Data.Text as T
import           Data.Char
import           Data.Monoid
import           Data.Maybe (fromMaybe)
import           Data.Hashable

import qualified Data.Map  as Map
import           Data.Map (Map)
import qualified Data.Aeson as Aeson
import           Data.Aeson (ToJSON, FromJSON)
import           Data.List (groupBy)

import           GHC.Generics

-- | a quiz has a preamble and a list of questions
data Quiz
  = Quiz { preamble :: [P.Block]         -- ^ description preamble
         , questions :: [Question]       -- ^ list of questions
         }

data Question
  = Question { identifier :: Text
             , description :: [P.Block]
             , choices :: Choices
             }

data Choices
  = FillIn Text (Text -> Text)
  -- ^ fill-in answer key and a normalization function
  | Alternatives Selection P.ListAttributes Alts
  -- ^ list of multiple choices

data Selection = Single | Multiple

type Alts =  [(Bool, [P.Block])]
  -- ^ alternatives: right/wrong and description

-- | answers to a quiz
-- mapping from question identifier to (possibly many) answers
newtype Answers = Answers (Map Text [Key])
  deriving (Show, Semigroup, Monoid, ToJSON, FromJSON)


answerKeys :: Choices -> [Key]
answerKeys (FillIn key _)
  = [key]
answerKeys (Alternatives _ attrs alts)
  = [key | (key, (True,_)) <- zip (listLabels attrs) alts]


type Key = Text

-- | a quiz together with answers
data QuizAnswers
  = QuizAnswers { quiz :: Text
                , answers :: Answers
                } deriving (Generic, Show)

instance ToJSON QuizAnswers where
  toEncoding = Aeson.genericToEncoding Aeson.defaultOptions

instance FromJSON QuizAnswers  -- derived implementation

-- | lookup selections for a specific question
lookupAnswer :: Question -> Answers -> [Key]
lookupAnswer Question{..} (Answers hm)
  = fromMaybe [] $ Map.lookup identifier hm

-- | convert an already shuffled quiz into a Pandoc document
quizToDocument :: Quiz -> P.Pandoc
quizToDocument Quiz{..}
  = P.doc (P.fromList preamble <> mconcat (map questionDoc questions))

questionDoc :: Question -> P.Blocks
questionDoc Question{..}
  =  P.fromList (choicesHeader description (answerKeys choices)) <>
     choicesAlts choices

choicesHeader
  (P.Header n (id,classes,kvs) inlines : rest) answers
  = P.Header n (id,classes,kvs') inlines : rest
  where kvs' = [("answers", a) | a<-answers] ++ kvs
choicesHeader _ _
  = error "choicesHeader: question must begin with header"

choicesAlts :: Choices -> P.Blocks
choicesAlts (FillIn _ _) = mempty
choicesAlts (Alternatives _ attrs alts)
  = P.orderedListWith attrs [P.fromList blocks | (_, blocks)<-alts]


quizToText :: Quiz -> Text
quizToText q =
  case P.runPure (P.writeMarkdown pandocWriterOptions $ quizToDocument q)
  of
    Left err -> T.pack (show err)
    Right txt -> txt

pandocWriterOptions :: P.WriterOptions
pandocWriterOptions
  = P.def { P.writerExtensions = P.pandocExtensions
          , P.writerSetextHeaders = False
          }


-- | deterministic shuffle questions & answers in a quiz
shuffleQuiz :: UserLogin -> Page -> Quiz
shuffleQuiz uid page
  = Rand.run (seed+salt) (makeQuiz page >>= shuffle1 >>= shuffle2)
  where
    meta = pageMeta page
    seed = fromMaybe (hash uid) $ lookupFromMeta "shuffle-seed" meta
    salt = fromMaybe 0 $ lookupFromMeta "shuffle-salt" meta
    opt1 = fromMaybe False $ lookupFromMeta "shuffle-questions" meta
    opt2 = fromMaybe False $ lookupFromMeta "shuffle-answers" meta
    shuffle1 = if opt1 then shuffleQuestions else return
    shuffle2 = if opt2 then shuffleAlternatives else return


-- | split up a list of blocks into questions
makeQuiz :: Page -> Rand Quiz
makeQuiz (P.Pandoc _ blocks)
  = Quiz blocks' <$> mapM Rand.choose groups
  where
    blocks' = takeWhile (not.isQuestion) blocks
    blocks''= dropWhile (not.isQuestion) blocks
    groups  = groupQuestions (makeQuestions blocks'')

-- | group together questions with the same group attribute
groupQuestions :: [Question] -> [[Question]]
groupQuestions
  = groupBy (\q q' -> name q `eq` name q')
  where
    name = lookup "group" . headerAttrs . questionHeader
    eq (Just n) (Just n') = n == n'
    eq _        _         = False

-- | split a list of blocks into questions
makeQuestions :: [P.Block] -> [Question]
makeQuestions [] = []
makeQuestions (block : blocks)
  | isQuestion block =
    makeQuestion block blocks' : makeQuestions blocks''
  | otherwise =
    makeQuestions blocks''
  where
    blocks'  = takeWhile (not . isQuestion) blocks
    blocks'' = dropWhile (not . isQuestion) blocks



-- | get the header block from a question
questionHeader :: Question -> P.Block
questionHeader Question{..} = head description


-- | check if a block is a question header
isQuestion :: P.Block -> Bool
isQuestion b = "question" `elem` headerClasses b

-- | get class atributes for a header block
headerClasses :: P.Block -> [Text]
headerClasses (P.Header _ (_, classes, _) _) = classes
headerClasses _                            = []

headerAttrs :: P.Block -> [(Text,Text)]
headerAttrs (P.Header _ (_, _, kvs) _) = kvs
headerAttrs _                        = []

headerIdent :: P.Block -> Maybe Text
headerIdent (P.Header _ (id, _,_) _) = Just id
headerIdent _                        = Nothing

removeKey :: Text -> P.Block -> P.Block
removeKey k (P.Header n (id, classes, kvs) inlines)
  = P.Header n (id, classes, filter (\(k',_) ->  k' /= k) kvs) inlines
removeKey _ b = b


makeQuestion :: P.Block -> [P.Block] -> Question
makeQuestion header rest
  = Question { identifier = fromMaybe "" (headerIdent header)
             , description = (header':prefix) ++ posfix
             , choices =
                 if  "fillin" `elem` headerClasses header
                 then FillIn (T.concat answers) normalize
                 else Alternatives multiples attrs alts
             }
  where
    header' = removeKey "answer" header
    answers = [a | ("answer",a) <- headerAttrs header]
    normalize = T.filter (not.isSpace)
    prefix = takeWhile (not . isList) rest
    posfix = drop 1 $ dropWhile (not . isList) rest
    (attrs,items) = fromMaybe (emptyAttrs,[]) $
                    getFirst $ P.query getList rest
    alts = [ (truth, item)
           | (label, item) <- zip (listLabels attrs) items
           , let truth = label `elem` answers
           ]
    multiples
      | "multiple" `elem` headerClasses header = Multiple
      | otherwise                              = Single
    emptyAttrs = (1, P.DefaultStyle, P.DefaultDelim)
    isList (P.OrderedList _ _) = True
    isList _                   = False
    getList (P.OrderedList attrs items)  = First (Just (attrs, items))
    getList _                            = First Nothing



-- | shuffle questions and answers
shuffleQuestions :: Quiz -> Rand Quiz
shuffleQuestions (Quiz preamble questions)
  = Quiz preamble <$> Rand.shuffle questions

-- | shuffle alternatives in multiple choice questions
shuffleAlternatives :: Quiz -> Rand Quiz
shuffleAlternatives (Quiz preamble questions)
  = Quiz preamble <$> mapM shuffle questions
  where
    shuffle q@Question{..} = case choices of
      FillIn{} ->
        return q   -- no shuffling required
      Alternatives multiples attrs alts -> do
        alts' <- Rand.shuffle alts -- shuffle alternatives
        return q { choices = Alternatives multiples attrs alts' }

-----------------------------------------------------------
-- Pandoc stuff
-----------------------------------------------------------
-- | enumerate Pandoc list labels
listLabels :: P.ListAttributes -> [Text]
listLabels (start, style, _) = drop (start-1) $ listNumbers style

listNumbers :: P.ListNumberStyle -> [Text]
listNumbers P.LowerAlpha = map T.singleton ['a'..'z']
listNumbers P.UpperAlpha = map T.singleton ['A'..'Z']
listNumbers P.LowerRoman = lowerRomans
listNumbers P.UpperRoman = upperRomans
listNumbers _          = map (T.pack . show) [1::Int .. 100]


lowerRomans, upperRomans :: [Text]
lowerRomans = map (T.pack . makeRoman) [1..100]
upperRomans = map T.toUpper lowerRomans


makeRoman :: Int -> String
makeRoman 0 = ""
makeRoman n
  | n>0 = symbols ++ makeRoman (n - value)
  | otherwise = error "lowerRoman: negative argument"
  where (value, symbols) = head [ (val,sym) | (val,sym) <-numerals, val<=n ]

numerals :: [(Int, String)]
numerals =
  [ (1000, "m"), (900, "cm"), (500, "d"), (400, "cd"),
    (100, "c"), (90, "xc"), (50, "l"), (40, "xl"),
    (10, "x"),  (9, "ix"), (5, "v"), (4, "iv"), (1, "i") ]
