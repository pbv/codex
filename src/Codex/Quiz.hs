{-# LANGUAGE OverloadedStrings #-}
--
-- Multiple choice quizzes
--
module Codex.Quiz where

import           Data.Char
import           Text.Pandoc.Definition
import           Text.Pandoc.Walk
import           Data.Monoid
import           Data.Maybe (fromMaybe, catMaybes)
import           Data.Hashable
import qualified Data.Text as T
import           Data.Text(Text)
import           Data.ByteString.UTF8 (ByteString)
import qualified Data.ByteString.UTF8 as B

import           Codex.Quiz.Random
import           Codex.Page
import           Codex.Types

import           Snap.Core hiding (path)

import           Heist
import qualified Heist.Splices as I
import qualified Heist.Interpreted as I

import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Map            as Map
import           Data.Aeson
import           Data.Map.Syntax
import           Data.List (intersperse, sort)

import qualified Text.XmlHtml as X


-- | a quiz has a preamble blocks and a list of questions
data Quiz = Quiz [Block] [Question]
  deriving Show

type Label = String

-- | a question consists of a preamble (list of blocks), 
-- list of items describing options tagged with truth values
data Question = Question [Block] ListAttributes [(Label, Bool, [Block])]
  deriving Show


-- | answers to a quiz;
-- map from question id to (possibly many) selected options
newtype Answers = Answers (HashMap String [Label])
  deriving Show

emptyAnswers :: Answers
emptyAnswers = Answers HashMap.empty

-- | instances for converting to/from JSON
instance ToJSON Answers where
  toJSON (Answers m) = toJSON m

instance FromJSON Answers where
  parseJSON v = Answers <$> parseJSON v


-- | get all form parameters as a quiz answer
--
getAnswers :: MonadSnap m => m Answers
getAnswers = do
  m <- getParams
  let m'= HashMap.fromList [ (B.toString k, map B.toString vs)
                           | (k,vs)<-Map.assocs m ]
  return (Answers m')
  
lookupAnswers :: Question -> Answers -> [Label]
lookupAnswers (Question (header:_) _ _) (Answers hm) 
  = fromMaybe [] $ do name <- identifier header
                      HashMap.lookup name hm

-- | deterministic shuffle questions & answers in a quiz
shuffleQuiz :: UserLogin -> Page -> Quiz
shuffleQuiz uid page
  = runRand (shuffle2 =<< shuffle1 (toQuiz page)) seed'
  where
    meta = pageMeta page
    seed = fromMaybe (0 :: Int) (lookupFromMeta "seed" meta)
    seed'= hashWithSalt seed uid
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
makeQuestion header body
  = Question (header':preamble) attrs alts
  where
    preamble = takeWhile (not . list) body
    (attrs,items) = fromMaybe (emptyAttrs,[]) $
                    getFirst $ query answers body
    key = [v | ("answer", v) <- attributes header]
    alts = [ (label, truth, item)
           | (label,item) <- zip (listLabels attrs) items
           , let truth = label `elem` key
           ]
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
shuffleSingle (Question preamble attrs alts) = do
  alts' <- shuffle alts
  return (Question preamble attrs alts')


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
  [ (1000, "m"), (900, "cm"), (500, "d"), (400, "cd"),
    (100, "c"), (90, "xc"), (50, "l"), (40, "xl"),
    (10, "x"),  (9, "ix"), (5, "v"), (4, "iv"), (1, "i") ]


-----------------------------

quizSplices :: Monad m => Quiz -> Answers -> Splices (I.Splice m)
quizSplices (Quiz preamble questions) answers = do
  "quiz-preamble" ## return (blocksToHtml preamble)
  "questions" ## I.mapSplices (I.runChildrenWith . questionSplice answers) questions


-- | splices for questions with answer key
questionSplice :: Monad m => Answers -> Question -> Splices (I.Splice m)
questionSplice answers question@(Question preamble@(header:_) attrs alts) = do
  let multiples = "multiple" `elem` classes header
  let responses = lookupAnswers question answers
  let answers = sort [ label
                     | (label, (_,True,_)) <- zip (listLabels attrs) alts
                     ]
  "question-preamble" ## return (blocksToHtml preamble)
  "question-name" ## maybe (return []) (I.textSplice . T.pack) (identifier header)
  "list-type" ## I.textSplice (listType attrs)
  "list-start" ## I.textSplice (listStart attrs)
  "onclick-callback" ## if multiples then return []
                        else I.textSplice "onlyOne(this)"
  "answer-key" ## I.textSplice (T.pack $ concat $ intersperse "," answers)
  "alternatives" ##
    I.mapSplices (I.runChildrenWith . altSplices responses) alts
  where
    altSplices responses (label,truth,item) = do
      "alternative-label" ## I.textSplice (T.pack label)
      "alternative" ## return (blocksToHtml item)
      "if-checked" ## I.ifElseISplice (label `elem` responses)
      "if-correct" ## I.ifElseISplice truth
      

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
