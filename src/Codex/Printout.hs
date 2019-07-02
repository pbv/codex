{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}

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
import qualified Data.Text.IO as T
import           Control.Monad (forM)
import           Control.Monad.State

import qualified Data.Configurator as Configurator
import           Snap.Snaplet
import           Snap.Snaplet.Router

import           Codex.Utils
import           Codex.Types
import           Codex.Application
import           Codex.Handlers
import           Codex.Submission
import           Codex.Page
import           Codex.Tester.Result

import           Text.Pandoc hiding (getZonedTime)
import           Text.Pandoc.Builder

import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import           Data.List (sortBy)
import           Data.Function(on)


-- | generate Markdown printouts for best submissions
generatePrintouts :: Patterns -> Codex.Submission.Ordering -> Codex ()
generatePrintouts patts order = do
  conf <- getSnapletUserConfig
  dir <- liftIO $ Configurator.require conf "printouts.directory"
  liftIO $ createDirectoryIfMissing True dir
  templ <- liftIO $
           readFile =<< Configurator.require conf "printouts.template"
  let opts = def { writerTemplate =  Just templ
                 , writerExtensions = pandocExtensions
                 , writerSetextHeaders = False
                 , writerListings = True
                 }
  getSummary patts order >>= writePrintouts dir opts
  redirectURL (Page ["index.md"])


-- | cummulative summary of submissions
-- for each user for each exercise (path)
type Summary = HashMap UserLogin (HashMap FilePath Submission)

-- | best of two submissions
best :: Submission -> Submission -> Submission
best s1 s2
  = let class1 = resultStatus (submitResult s1)
        class2 = resultStatus (submitResult s2)
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


-- | generate printouts from the summary
writePrintouts :: FilePath -> WriterOptions -> Summary -> Codex [FilePath]
writePrintouts dir opts  summary = do
  forM (HM.toList summary) $
    \(uid, submap) -> do
        let filepath = dir </> T.unpack (fromLogin uid) <.> "md"
        let sorted = sortBy (compare`on`submitPath) $ HM.elems submap
        report <- userPrintout uid sorted
        case runPure (writeMarkdown opts report) of
          Right txt ->  do liftIO $ T.writeFile filepath txt
                           return filepath
          Left err -> error (show err)

    
    
userPrintout :: UserLogin -> [Submission] -> Codex Pandoc
userPrintout uid  submissions = do
  Handlers{handlePrintout} <- gets _handlers
  root <- getDocumentRoot
  now <- liftIO getZonedTime
  let login = T.unpack (fromLogin uid)
  fullname <- maybe login T.unpack <$> queryFullname uid
  blocks <- forM submissions $
            \sub -> do
              page <- readMarkdownFile (root </> submitPath sub)
              content <- handlePrintout uid page sub
              return (submissionPrintout page sub content)
  return (-- setTitle (text title) $
          setAuthors [text $ fullname ++ " (" ++ login ++ ")"] $
          setDate (text $ show now) $
          doc $ mconcat blocks)


submissionPrintout :: Page -> Submission -> Blocks -> Blocks
submissionPrintout page sub@Submission{..}  content
  = mconcat [ header 1 title
            , submissionHeader sub
            , content
            , codeBlock msg
            , horizontalRule ]
  where
    title = maybe (text submitPath) fromList (pageTitle page)
    msg = T.unpack $ resultReport submitResult

submissionHeader Submission{..}
  = header 2 (strong (text $ show $ resultStatus submitResult) <>
              space <>
              emph (text $ "(" ++ show (resultCheck submitResult) ++ ")")) <>
    para (text ("Submission " ++ show submitId ++ "; " ++
                 show submitTime ))


