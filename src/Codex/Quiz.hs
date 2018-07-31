{-# LANGUAGE OverloadedStrings #-}

module Codex.Quiz where

import           Data.Char
import           Text.Pandoc.Definition
import           Text.Pandoc.Walk
import           Data.Monoid
import           Data.Maybe (fromMaybe, catMaybes)
import           Data.Hashable
import qualified Data.Text as T
import           Data.Text(Text)
import           Codex.Quiz.Random
import           Codex.Page
import           Codex.Types

import           Heist
import qualified Heist.Splices as I
import qualified Heist.Interpreted as I
import           Data.Map.Syntax

import qualified Text.XmlHtml as X


-- | a quiz has a preamble blocks and a list of questions
data Quiz = Quiz [Block] [Question]
  deriving Show

-- | a question has a preamble blocks,
-- a list of answers (strings), and an ordered list of answers 
data Question = Question [Block] [String] ListAttributes [[Block]]
  deriving Show

makeQuiz :: UserLogin -> Page -> Quiz
makeQuiz uid page
  = runRand (shuffle2 =<< shuffle1 (toQuiz page)) salt
  where
    meta = pageMeta page
    salt = hashWithSalt salt' uid
    salt'= fromMaybe (0 :: Int) (lookupFromMeta "salt" meta)
    opt1 = fromMaybe False $ lookupFromMeta "shuffle-questions" meta
    opt2 = fromMaybe False $ lookupFromMeta "shuffle-answers" meta
    shuffle1 = if opt1 then shuffleQuestions else return
    shuffle2 = if opt2 then shuffleAnswers else return 
    

-- | split up a list of blocks into questions
toQuiz :: Page -> Quiz
toQuiz (Pandoc _ blocks) =  Quiz blocks' (questions blocks'')
  where
    blocks' = takeWhile (not . header) blocks
    blocks''= dropWhile (not . header) blocks




questions :: [Block] -> [Question]
questions [] = []
questions (block : blocks)
  | header block = makeQuestion block blocks' : questions blocks''
  | otherwise    = questions blocks
  where
    blocks'  = takeWhile (not . header) blocks
    blocks'' = dropWhile (not . header) blocks

-- | check if a block is a question header
header :: Block -> Bool
header = ("question" `elem`) . classes 

-- | class atributes for a header block
classes :: Block -> [String]
classes (Header _ (_, classes, _) _) = classes
classes _                            = []

attributes :: Block -> [(String,String)]
attributes (Header _ (_, _, kvs) _) = kvs
attributes _                        = []

identifier :: Block -> Maybe String
identifier (Header _ (id, _,_) _) = Just id
identifier _                      = Nothing

removeKey :: String -> Block -> Block
removeKey k (Header n (id, classes, kvs) inlines)
  = Header n (id, classes, filter (\(k',_) ->  k' /= k) kvs) inlines
removeKey _ b = b

makeQuestion :: Block -> [Block] -> Question
makeQuestion header body = Question (header':preamble) answer attrs items
  where
    preamble = takeWhile (not . list) body
    (attrs,items) = fromMaybe (emptyAttrs,[]) $ getFirst $ query answers body
    answer = [v | ("answer", v) <- attributes header]
    header' = removeKey "answer" header
    emptyAttrs = (1, DefaultStyle, DefaultDelim)
    list (OrderedList _ _) = True
    list _                 = False
    answers (OrderedList attrs items)  = First (Just (attrs, items))
    answers _                          = First Nothing


-- | shuffle questions and answers
shuffleQuestions :: Quiz -> Rand Quiz
shuffleQuestions (Quiz preamble questions)
  = Quiz preamble <$> shuffle questions

shuffleAnswers :: Quiz -> Rand Quiz
shuffleAnswers (Quiz preamble questions)
  = Quiz preamble <$> mapM shuffleSingle questions


shuffleSingle :: Question -> Rand Question
shuffleSingle (Question preamble answers attrs items) = do
  let labels = listLabels attrs
  labeled_items' <- shuffle (zip labels items)
  let labels' = map fst labeled_items'
  let items' = map snd labeled_items'
  let answers' = catMaybes $ map (flip lookup (zip labels labels')) answers
  return (Question preamble answers' attrs items')


-- | enumerate Pandoc list labels
listLabels :: ListAttributes -> [String]
listLabels (start, style, _) = drop (start-1) $ listNumbers style

listNumbers :: ListNumberStyle -> [String]
listNumbers LowerAlpha = map (:"") ['a'..'z']
listNumbers UpperAlpha = map (:"") ['A'..'Z']
listNumbers LowerRoman = lowerRomans
listNumbers UpperRoman = upperRomans
listNumbers _          = map show [1::Int .. 100]


lowerRomans, upperRomans :: [String]
lowerRomans = map makeRoman [1..100]
upperRomans = map (map toUpper) lowerRomans


makeRoman :: Int -> String
makeRoman 0 = ""
makeRoman n
  | n>0 = symbols ++ makeRoman (n - value)
  | otherwise = error "lowerRoman: negative argument"
  where (value, symbols) = head $ dropWhile ((>n).fst) numerals

numerals :: [(Int, String)]
numerals =
  [(1000, "m"), (900, "cm"), (500, "d"), (400, "cd"),
   (100, "c"), (90, "xc"), (50, "l"), (40, "xl"),
   (10, "x"),  (9, "ix"), (5, "v"), (4, "iv"), (1, "i")]


  


-----------------------------

quizSplices :: Monad m => Quiz -> Splices (I.Splice m)
quizSplices (Quiz preamble questions) = do
  "quiz-preamble" ## return (blocksToHtml preamble)
  "questions" ## I.mapSplices (I.runChildrenWith . questionSplice) questions


questionSplice :: Monad m => Question -> Splices (I.Splice m)
questionSplice (Question preamble answers attrs items) = do
  "answer-preamble" ## return (blocksToHtml preamble)
  "answer-name" ## maybe (return []) (I.textSplice . T.pack) (identifier $ head preamble)
  "list-type" ## I.textSplice (listType attrs)
  "list-start" ## I.textSplice (listStart attrs)
  "answers" ## I.mapSplices (I.runChildrenWith . answerSplices) (zip items (listLabels attrs))
  where 
    answerSplices (block, label) = do
      "answer-item" ## return (blocksToHtml block)
      "answer-label" ## I.textSplice (T.pack label)


listType :: ListAttributes -> Text
listType (_, style, _)
  = case style of
      LowerAlpha -> "a"
      UpperAlpha -> "A"
      LowerRoman -> "i"
      UpperRoman -> "I"
      _          -> "1"

listStart :: ListAttributes  -> Text
listStart (n, _, _) = T.pack (show n)
