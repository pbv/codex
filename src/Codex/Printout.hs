{-# LANGUAGE OverloadedStrings, RecordWildCards #-} 
{-
   Produce printouts for exams, etc.
-}
module Codex.Printout(
  handlePrintouts
  ) where

import           System.FilePath
import           Data.Time.LocalTime
import           System.Directory

import qualified Data.Text as T
import           Control.Monad (unless, forM_)
import           Control.Monad.Trans
import           Control.Applicative

import qualified Data.Configurator as Configurator
import           Snap.Core(writeText)
import           Snap.Snaplet
import           Snap.Snaplet.Auth

import           Codex.Utils
import           Codex.Types
import           Codex.Application
import           Codex.Submission
import           Codex.Page
import           Codex.Tester.Result

import           Text.Pandoc
import           Text.Pandoc.Builder

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M


import           Data.Version                                (showVersion)
import           Paths_codex                                 (version)



{-
handlePrintouts :: Codex ()
handlePrintouts = withAdmin $ do
  -- usr <- require (with auth currentUser) <|> unauthorized
  -- unless (isAdmin usr) unauthorized  
  -- fetch configuration options
  conf <- getSnapletUserConfig
  dir <- liftIO $ Configurator.require conf "printouts.directory"
  liftIO $ createDirectoryIfMissing True dir
  templ <- liftIO $ readFile =<< Configurator.require conf "printouts.template"
  let opts = def { -- writerStandalone = True
                   writerTemplate = Just templ
                 , writerHighlight = True
                 , writerSetextHeaders = False
                 }  
  uids <- querySubmissionUsers
  writeText $ "Generating " <> T.pack (show $ length uids) <> " printouts:"
  forM_ uids $ \uid -> do
    writeText ("\t" <> fromLogin uid)
    rep <- userReport uid
    let filepath = dir </> T.unpack (fromLogin uid) <.> "md"
    liftIO $ writeFile filepath (writeMarkdown opts rep)
  writeText "\nDone!\n"


-- | report submissions for a single user
userReport :: UserLogin -> Codex Pandoc
userReport uid = do
  base <- getDocumentRoot
  now <- liftIO getZonedTime
  let login = T.unpack $ fromLogin uid
  fullname <- maybe login T.unpack <$> queryFullname uid
  -- fetch all submitted exercise paths
  exs <- querySubmissionPaths uid
  list <- mapM (exerciseReport uid base) exs
  let title = "Codex " ++ showVersion version
  return (setTitle (text title) $
          setAuthors [text $ fullname ++ " (" ++ login ++ ")"] $
          setDate (text $ show now) $
          doc $ mconcat list
         )


-- | report best submission for a user an exercise
exerciseReport :: UserLogin -> FilePath -> FilePath -> Codex Blocks
exerciseReport uid base path = do
  page <- liftIO $ readMarkdownFile (base </> path)
  opt <- getFinal uid path
  let title = maybe (text path) fromList (pageTitle page)
  return (maybe mempty (report title) opt)
  where
    report title Submission{..}
      = let lang = T.unpack $ fromLanguage $ codeLang submitCode
            submitted = T.unpack $ codeText submitCode
            classify = show $ resultClassify submitResult
            msg = T.unpack $ resultMessage submitResult
        in
          header 1 title <>
          header 2 (strong (text classify) <>
                    space <>
                    emph (text $ "(" ++ show submitTiming ++ ")")) <>
          para (text ( "Submission " ++ show submitId ++
                       "; " ++ show submitTime)) <>
          codeBlockWith ("", [lang, "numberLines"], []) submitted <>
          codeBlock msg <>
          horizontalRule


-- | get last accepted submission (if any), otherwise the last submission
getFinal :: UserLogin -> FilePath -> Codex (Maybe Submission)
getFinal uid ex = do
  s1 <- getLastAccepted uid ex
  s2 <- getLastSubmitted uid ex
  return (s1 <|> s2)
-}

-- | produce printouts for submissions
handlePrintouts :: Patterns -> Codex.Submission.Ordering -> Codex ()
handlePrintouts patts order = do
  conf <- getSnapletUserConfig
  dir <- liftIO $ Configurator.require conf "printouts.directory"
  liftIO $ createDirectoryIfMissing True dir
  templ <- liftIO $ readFile =<< Configurator.require conf "printouts.template"
  let opts = def { -- writerStandalone = True
                   writerTemplate = Just templ
                 , writerHighlight = True
                 , writerSetextHeaders = False
                 }
  generateSummary patts order >>= generateReports dir opts 


-- | cummulative summary of submissions
-- for each user, for each exercise (path)
type Summary = Map UserLogin (Map FilePath Submission)

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
  = M.insertWith (M.unionWith best) submitUser (M.singleton submitPath sub) summary
  

-- | generate a summary from submissions in the DB
generateSummary :: Patterns -> Codex.Submission.Ordering -> Codex Summary
generateSummary patts order 
  = withFilterSubmissions patts order M.empty (\x y -> return (addSubmission x y))

-- | generate reports from the summary
generateReports :: FilePath -> WriterOptions -> Summary -> Codex ()
generateReports dir opts summary
  = forM_ (M.assocs summary) $
    (\(uid,submap) ->
        do writeText (fromLogin uid <> "\n")
           let filepath = dir </> T.unpack (fromLogin uid) <.> "md"
           report <- userReport uid (M.assocs submap)
           liftIO $ writeFile filepath (writeMarkdown opts report))

    
userReport :: UserLogin -> [(FilePath,Submission)] -> Codex Pandoc
userReport uid submissions = do
  now <- liftIO getZonedTime
  let login = T.unpack (fromLogin uid)
  fullname <- maybe login T.unpack <$> queryFullname uid
  titles <- sequence [ getTitle path | (path,_) <- submissions ]
  let exercises = [ submissionReport sub | (_,sub) <- submissions ]
  let blocks = [ header 1 title <> report | (title,report) <- zip titles exercises]
  return (setTitle (text "Report") $
          setAuthors [text $ fullname ++ "(" ++ login ++ ")"] $
          setDate (text $ show now) $
          doc $ mconcat blocks)


submissionReport :: Submission -> Blocks
submissionReport Submission{..} 
  = let lang = T.unpack $ fromLanguage $ codeLang submitCode
        submitted = T.unpack $ codeText submitCode
        classify = show $ resultClassify submitResult
        msg = T.unpack $ resultMessage submitResult
  in header 2 (strong (text classify) <>
               space <>
               emph (text $ "(" ++ show submitTiming ++ ")")) <>
     para (text ("Submission " ++ show submitId ++ "; " ++ show submitTime)) <>
     codeBlockWith ("", [lang, "numberLines"], []) submitted <>
     codeBlock msg <>
     horizontalRule


getTitle :: FilePath -> Codex Inlines
getTitle path = do opt <- getTitle' path
                   return (maybe (text path) fromList opt)

getTitle' :: FilePath -> Codex (Maybe [Inline])
getTitle' path = do
  base <- getDocumentRoot
  page <- liftIO $ readMarkdownFile (base </> path)
  return (pageTitle page)
  
