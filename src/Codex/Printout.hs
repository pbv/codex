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

import           Data.Version                                (showVersion)
import           Paths_codex                                 (version)



-- | produce printouts for final submissions
  -- ensure that a user with admin privileges is logged in
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
