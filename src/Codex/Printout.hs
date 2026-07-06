{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}

{-
   Produce printouts for exams
-}
module Codex.Printout(
  generatePrintouts
  ) where

import           System.FilePath
import           System.Process (callProcess)
import           Data.Time.LocalTime
import           System.Directory

import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Data.Maybe ( fromMaybe )
import           Control.Monad.State
import           Control.Exception

import qualified Data.Configurator as Configurator
import           Snap.Snaplet

import           Snap.Util.FileServe(serveFile)

import           Codex.Utils
import           Codex.Policy (formatLocalTime)
import           Codex.Types
import           Codex.Application
import           Codex.Handlers
import           Codex.Submission ( Submission(..),
                                    Validity(..),
                                    getUsersSubmitted,
                                    getBestSubmissions )
import           Codex.Page
import           Codex.Tester.Result

import           Text.Pandoc hiding (getCurrentTimeZone, getZonedTime)
import           Text.Pandoc.Builder
import           Data.Functor.Identity



-- | generate Markdown printouts for best submissions
generatePrintouts :: Codex ()
generatePrintouts = do
  conf <- getSnapletUserConfig
  dir <- liftIO $ Configurator.require conf "printouts.directory"
  liftIO $ removePathForcibly dir
  liftIO $ createDirectory dir
  tplPath <- liftIO $ Configurator.require conf "printouts.template"
  tplOut <- liftIO $ runIO $ getTemplate tplPath
  let tplText = case tplOut of
                    Right txt -> txt
                    Left err -> error (show err)
  let tpl = case runIdentity (compileTemplate tplPath tplText) of
              Right tpl -> tpl
              Left err -> error (show err)
  let opts = def { writerTemplate = Just tpl
                 , writerExtensions = pandocExtensions
                 , writerSetextHeaders = False
                 , writerListings = True
                 }
  writePrintouts dir opts
  liftIO $ callProcess "/usr/bin/tar" ["cf", "printouts.tar", dir]
  serveFile "printouts.tar"




writePrintouts :: FilePath -> WriterOptions -> Codex ()
writePrintouts dir opts = do
  uids <- getUsersSubmitted
  forM_ uids $ \uid -> do
    subs <- getBestSubmissions uid
    report <- userPrintout uid subs
    let filepath = dir </> T.unpack (fromLogin uid) <.> "md"
    liftIO $ case runPure (writeMarkdown opts report) of
      Right txt -> T.writeFile filepath txt
      Left err -> throwIO $ userError $ show err


userPrintout :: UserLogin -> [Submission] -> Codex Pandoc
userPrintout uid  submissions = do
  Handlers{handlePrintout} <- gets _handlers
  root <- getDocumentRoot
  tz <- liftIO getCurrentTimeZone
  now <- liftIO getZonedTime
  let login = fromLogin uid
  fullname <- fromMaybe login <$> queryUserMeta uid "fullname"
  optLang <- fmap T.unpack <$> queryUserMeta uid "translate"
  blocks <- forM submissions $
            \sub -> do
              let original = root </> submitPath sub
              page <- readMarkdownFile original optLang
              content <- handlePrintout uid page sub
              return (submissionPrintout tz page sub content)
  return (setAuthors [text $ fullname <> " (" <> login <> ")"] $
          setDate (text $ T.pack $ show now) $
          doc $ mconcat blocks)


submissionPrintout :: TimeZone -> Page -> Submission -> Blocks -> Blocks
submissionPrintout tz page Submission{..}  content
  = mconcat [ header 1 title
            , if verbose then
                fromList (pageUntitledBlocks page)
                else
                mempty
            , summary
            , content
            , horizontalRule
            ]
  where
    title = maybe (text $ T.pack submitPath) fromList (pageTitle page)
    summary
      = header 2 (strong (text $ T.pack $ show $ resultStatus submitResult) <>
                  space <>
                  emph (text $ "(" <> checkText submitCheck <> ")")) <>
        para (text ("Submission " <> T.pack (show submitId) <> "; " <>
                    formatLocalTime tz submitTime)
             )
    verbose = fromMaybe False
              (lookupFromMeta "verbose-printout" (pageMeta page))

checkText :: Validity -> Text
checkText Valid         = "Valid"
checkText (Invalid msg) = "Invalid: " <>  msg

