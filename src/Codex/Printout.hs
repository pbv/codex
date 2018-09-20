{-# LANGUAGE OverloadedStrings, RecordWildCards #-} 
{-
   Produce printouts for exams, etc.
-}
module Codex.Printout(
  generatePrintouts
  ) where

import           System.FilePath
import           Data.Time.LocalTime
import           System.Directory

import qualified Data.Text as T
import           Control.Monad (forM, forM_)
import           Control.Monad.Trans

import qualified Data.Configurator as Configurator
import           Snap.Core(writeText)
import           Snap.Snaplet

import           Codex.Utils
import           Codex.Types
import           Codex.Application
import           Codex.Submission
import           Codex.Page
import           Codex.Quiz hiding (header)
import           Codex.Tester.Result

import           Text.Pandoc
import           Text.Pandoc.Builder
import           Text.Pandoc.Walk

import           Data.Maybe
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM



-- | generate Markdown printouts for best submissions
generatePrintouts :: Patterns -> Codex.Submission.Ordering -> Codex ()
generatePrintouts patts order = do
  conf <- getSnapletUserConfig
  dir <- liftIO $ Configurator.require conf "printouts.directory"
  liftIO $ createDirectoryIfMissing True dir
  templ <- liftIO $
           readFile =<< Configurator.require conf "printouts.template"
  -- header <- liftIO $
  --           readMarkdownFile =<<
  --           Configurator.require conf "printouts.header"
  let opts = def { writerTemplate =  Just templ
                 , writerHighlight = True
                 , writerSetextHeaders = False
                 }
  getSummary patts order >>= writeReports dir opts
  return ()


-- | cummulative summary of submissions
-- for each user for each exercise (path)
type Summary = HashMap UserLogin (HashMap FilePath Submission)

-- | best of two submissions
best :: Submission -> Submission -> Submission
best s1 s2
  = let class1 = resultClassify (submitResult s1)
        class2 = resultClassify (submitResult s2)
    in case (class1, class2) of
         (Accepted, Accepted) -> latest s1 s2
         (Accepted, _)        -> s1
         (_       , Accepted) -> s2
         (_       , _)        -> latest s1 s2

latest :: Submission -> Submission -> Submission
latest s1 s2
  = if submitTime s1 < submitTime s2 then s2 else s1


-- | add a submision into the summary
addSubmission :: Summary -> Submission -> Summary
addSubmission summary sub@Submission{..}
  = HM.insertWith (HM.unionWith best) submitUser (HM.singleton submitPath sub) summary
  

-- | get a summary of relevant submissions in the DB
getSummary :: Patterns -> Codex.Submission.Ordering -> Codex Summary
getSummary patts order 
  = withFilterSubmissions patts order HM.empty (\x y -> return (addSubmission x y))

{-
-- | get all full name for collected users
getNames :: [UserLogin] -> Codex Names
getNames users = do
  names <- forM users $ \uid ->
    fromMaybe (fromLogin uid) <$> queryFullname uid
  return $ M.fromList $ zip users names
-}

-- | generate reports from the summary
writeReports :: FilePath -> WriterOptions -> Summary -> Codex [FilePath]
writeReports dir opts  summary = do
  forM (HM.toList summary) $
    \(uid, submap) -> do
        let filepath = dir </> T.unpack (fromLogin uid) <.> "md"
        report <- userReport uid (HM.elems submap)
        liftIO $ writeFile filepath (writeMarkdown opts report)
        return filepath

    
    
userReport :: UserLogin -> [Submission] -> Codex Pandoc
userReport uid  submissions = do
  root <- getDocumentRoot
  now <- liftIO getZonedTime
  let login = T.unpack (fromLogin uid)
  fullname <- maybe login T.unpack <$> queryFullname uid
  blocks <- forM submissions $
            \sub -> do
              page <- readMarkdownFile (root </> submitPath sub) 
              return (submissionReport page sub)
  return (-- setTitle (text title) $
          setAuthors [text $ fullname ++ " (" ++ login ++ ")"] $
          setDate (text $ show now) $
          doc $ mconcat blocks)


submissionReport :: Page -> Submission -> Blocks
submissionReport page sub@Submission{..}  
  = mconcat [ header 1 title
            , submissionHeader sub
            , content
            , codeBlock msg
            , horizontalRule ]
  where
    title = maybe (text submitPath) fromList (pageTitle page)
    content = if isQuiz page
              then quizReport quiz answers 
              else codeReport page sub 
    quiz = makeQuiz page
    answers = fromMaybe emptyAnswers $
              decodeAnswers $ codeText $ submitCode
    msg = T.unpack $ resultMessage submitResult

submissionHeader Submission{..}
  = header 2 (strong (text $ show $ resultClassify submitResult) <>
              space <>
              emph (text $ "(" ++ show submitTiming ++ ")")) <>
    para (text ("Submission " ++ show submitId ++ "; " ++
                 show submitTime ))

codeReport page Submission{..}
  = let 
        lang = T.unpack $ fromLanguage $ codeLang submitCode
        code = T.unpack $ codeText submitCode
  in codeBlockWith ("", [lang, "numberLines"], []) code

quizReport (Quiz _ questions) answers 
  = mconcat (map (flip questionReport answers) questions)


questionReport :: Question -> Answers -> Blocks
questionReport question@(Question preamble attrs alts) answers
  = let checked = lookupAnswers question answers
    in fromList preamble  <>
       orderedListWith attrs [ questionItem checked alt | alt <- alts ]


questionItem checked (label, truth, blocks)
  = label1 <> fromList blocks <> label2
  where
    reply = label `elem` checked
    label1 = if reply then plain (math "\\bullet")
             else plain (math "\\circ")
    label2 | reply && truth = plain $ strong $ text "OK"
           | reply && not truth = plain $ strong $ text "WRONG"
           | not reply && truth = plain $ strong $ text "MISS"
           | otherwise = mempty
    

