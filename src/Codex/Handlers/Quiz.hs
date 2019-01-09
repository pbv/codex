{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

--
-- Multiple choice quizzes
--
module Codex.Handlers.Quiz
  ( Quiz(..)
  , Question(..)
  , Answers(..)
  , makeQuiz
  , lookupAnswers
  , decodeAnswers
  , emptyAnswers
  , quizHandlers
  ) where

import           Codex.Types
import           Codex.Page
import           Codex.Application
import           Codex.Submission
import           Codex.Evaluate
import           Codex.Utils
import           Codex.Handlers
import           Codex.Tester.Result
import           Codex.Handlers.Quiz.Random

import           Snap.Core hiding (path)
import           Snap.Snaplet.Heist
import           Snap.Snaplet.Router

import           Heist
import qualified Heist.Splices as I
import qualified Heist.Interpreted as I

import qualified Text.Pandoc.Definition as P
import qualified Text.Pandoc.Builder as P
import qualified Text.Pandoc.Walk as P

import qualified Text.XmlHtml as X

import           Data.Char
import           Data.Monoid
import           Data.Maybe (fromMaybe, catMaybes)
import           Data.Hashable
import qualified Data.Text           as T
import qualified Data.Text.Encoding as T
import           Data.Text(Text)
import           Data.ByteString.UTF8 (ByteString)
import qualified Data.ByteString.UTF8 as B
import qualified Data.ByteString.Lazy as LB

import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Map            as Map
import           Data.Aeson (ToJSON, FromJSON, toJSON, parseJSON)
import qualified Data.Aeson          as Aeson
import           Data.Map.Syntax
import           Data.List (intersperse, sort)
import           Data.Time.LocalTime

import           Control.Monad (guard)
import           Control.Monad.IO.Class (liftIO)

-- | a quiz has a preamble and a list of questions
data Quiz = Quiz [P.Block] [Question]
  deriving Show

-- | a question consists of a preamble (list of blocks), 
-- list of items describing options tagged with truth values
data Question
  = Question [P.Block] P.ListAttributes  [(Label, Bool, [P.Block])]
  deriving Show

-- | answer labels
type Label = String


-- | answers to a quiz;
-- map from question id to (possibly many) selected options
newtype Answers = Answers (HashMap String [Label])
  deriving (Show, Semigroup, Monoid)

emptyAnswers :: Answers
emptyAnswers = Answers HashMap.empty

-- | instances for converting to/from JSON
instance ToJSON Answers where
  toJSON (Answers m) = toJSON m

instance FromJSON Answers where
  parseJSON v = Answers <$> parseJSON v


-- | convert quiz answers from/to text
decodeAnswers :: Text -> Maybe Answers
decodeAnswers = Aeson.decode . LB.fromStrict . T.encodeUtf8 

encodeAnswers :: Answers -> Text
encodeAnswers = T.decodeUtf8 . LB.toStrict . Aeson.encode 


-- | get quiz answers from form parameters 
--
getFormAnswers :: MonadSnap m => m Answers
getFormAnswers = do
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
  = runRand (shuffle2 =<< shuffle1 (makeQuiz page)) seed'
  where
    meta = pageMeta page
    seed = fromMaybe (0 :: Int) (lookupFromMeta "shuffle-seed" meta)
    seed'= hashWithSalt seed uid
    opt1 = fromMaybe False $ lookupFromMeta "shuffle-questions" meta
    opt2 = fromMaybe False $ lookupFromMeta "shuffle-answers" meta
    shuffle1 = if opt1 then shuffleQuestions else return
    shuffle2 = if opt2 then shuffleAnswers else return 
    

-- | split up a list of blocks into questions
makeQuiz :: Page -> Quiz
makeQuiz (P.Pandoc _ blocks) =  Quiz blocks' (questions blocks'')
  where
    blocks' = takeWhile (not . header) blocks
    blocks''= dropWhile (not . header) blocks


questions :: [P.Block] -> [Question]
questions [] = []
questions (block : blocks)
  | header block = makeQuestion block blocks' : questions blocks''
  | otherwise    = questions blocks
  where
    blocks'  = takeWhile (not . header) blocks
    blocks'' = dropWhile (not . header) blocks

-- | check if a block is a question header
header :: P.Block -> Bool
header = ("question" `elem`) . classes 

-- | class atributes for a header block
classes :: P.Block -> [String]
classes (P.Header _ (_, classes, _) _) = classes
classes _                            = []

attributes :: P.Block -> [(String,String)]
attributes (P.Header _ (_, _, kvs) _) = kvs
attributes _                        = []

identifier :: P.Block -> Maybe String
identifier (P.Header _ (id, _,_) _) = Just id
identifier _                        = Nothing

removeKey :: String -> P.Block -> P.Block
removeKey k (P.Header n (id, classes, kvs) inlines)
  = P.Header n (id, classes, filter (\(k',_) ->  k' /= k) kvs) inlines
removeKey _ b = b

makeQuestion :: P.Block -> [P.Block] -> Question
makeQuestion header body
  = Question (header':preamble) attrs alts
  where
    preamble = takeWhile (not . list) body
    (attrs,items) = fromMaybe (emptyAttrs,[]) $
                    getFirst $ P.query answers body
    key = [v | ("answer", v) <- attributes header]
    alts = [ (label, truth, item)
           | (label,item) <- zip (listLabels attrs) items
           , let truth = label `elem` key
           ]
    header' = removeKey "answer" header
    emptyAttrs = (1, P.DefaultStyle, P.DefaultDelim)
    list (P.OrderedList _ _) = True
    list _                 = False
    answers (P.OrderedList attrs items)  = First (Just (attrs, items))
    answers _                            = First Nothing


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
listLabels :: P.ListAttributes -> [String]
listLabels (start, style, _) = drop (start-1) $ listNumbers style

listNumbers :: P.ListNumberStyle -> [String]
listNumbers P.LowerAlpha = map (:"") ['a'..'z']
listNumbers P.UpperAlpha = map (:"") ['A'..'Z']
listNumbers P.LowerRoman = lowerRomans
listNumbers P.UpperRoman = upperRomans
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
  let answers = fromMaybe emptyAnswers $ case subs of
                  [] -> Nothing
                  _ ->  getSubmittedAnswers (last subs)
  withTimeSplices page $ renderWithSplices "_quiz" $ quizSplices quiz answers


getSubmittedAnswers :: Submission -> Maybe Answers
getSubmittedAnswers = decodeAnswers . codeText . submitCode

quizSubmit :: UserLogin -> FilePath -> Page -> Codex ()
quizSubmit uid rqpath page = do
  guard (isQuiz page)
  ans <- getFormAnswers
  let text = encodeAnswers ans
  sid <- newSubmission uid rqpath (Code "json" text)
  redirectURL (Report sid)

isQuiz :: Page -> Bool
isQuiz page = pageTester page == Just "quiz" 

-- | report a quiz submission
quizReport :: FilePath -> Page -> Submission -> Codex ()
quizReport rqpath page sub = do
  guard (isQuiz page)
  tz <- liftIO getCurrentTimeZone
  let quiz = shuffleQuiz (submitUser sub) page
  let answers = fromMaybe emptyAnswers $ getSubmittedAnswers sub
  renderWithSplices "_answers" $ do
    urlSplices rqpath
    pageSplices page
    feedbackSplices page
    submitSplices tz sub
    quizSplices quiz answers

-- 
-- | handler for generating a quiz printout 
--
quizPrintout :: UserLogin -> Page -> Submission -> Codex P.Blocks
quizPrintout _ page sub@Submission{..}  = do
  guard (isQuiz page) 
  return mempty
    {- $
    mconcat [ -- P.header 1 title
            -- , ppHeader sub
             -- ppQuiz quiz answers 
             --  P.codeBlock msg
            --  P.horizontalRule
            ] -}
  where
    -- title = maybe (P.text submitPath) P.fromList (pageTitle page)
    quiz = makeQuiz page
    answers = fromMaybe emptyAnswers $
              decodeAnswers $ codeText $ submitCode
    msg = T.unpack $ resultMessage submitResult

{-
ppHeader Submission{..}
  = P.header 2 (P.strong (P.text $ show $ resultClassify submitResult) <>
              P.space <>
              P.emph (P.text $ "(" ++ show submitTiming ++ ")")) <>
    P.para (P.text ("Submission " ++ show submitId ++ "; " ++
                    show submitTime))
-}

ppQuiz :: Quiz -> Answers -> P.Blocks
ppQuiz (Quiz _ questions) answers 
  = mconcat (map (flip ppQuestion answers) questions)

ppQuestion :: Question -> Answers -> P.Blocks
ppQuestion question@(Question preamble attrs alts) answers
  = let checked = lookupAnswers question answers
    in P.fromList preamble  <>
       P.orderedListWith attrs [ questionItem checked alt | alt <- alts ]

questionItem checked (label, truth, blocks)
  = label1 <> P.fromList blocks <> label2
  where
    reply = label `elem` checked
    label1 = P.plain $ P.math $ if reply then "\\bullet" else "\\circ"
    label2 | reply && truth = P.plain $ P.strong $ P.text "OK"
           | reply && not truth = P.plain $ P.strong $ P.text "WRONG"
           | not reply && truth = P.plain $ P.strong $ P.text "MISS"
           | otherwise = mempty
    
-- | record with quiz handlers
quizHandlers :: Handlers Codex
quizHandlers
  = Handlers quizView quizSubmit (const quizReport) quizPrintout

 
