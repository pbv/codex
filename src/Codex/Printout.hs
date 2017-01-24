{-# LANGUAGE OverloadedStrings, RecordWildCards #-} 
{-
   Produce printouts for exams, etc.
-}
module Codex.Printout(
  handlePrintout
  ) where
-- import           Prelude hiding (catch)
import           System.FilePath
-- import           System.Directory
-- import           System.Process
-- import           System.IO.Error


import qualified Data.Text as T
import           Control.Monad.Trans
import           Control.Applicative
import           Data.Maybe(fromMaybe)

import           Codex.Utils
import           Codex.Types
import           Codex.Application
import           Codex.Submission
import           Codex.Page
import           Codex.Tester.Result

import           Text.Pandoc
import           Text.Pandoc.Builder


handlePrintout :: Codex ()
handlePrintout = do
  rep <- userReport "pedro"
  liftIO $ writeFile "pedro.md" (writeMarkdown def rep)



userReport :: UserLogin -> Codex Pandoc
userReport uid = do
  base <- getDocumentRoot
  fullname <- maybe (show uid) T.unpack <$> queryFullname uid
  exs <- allSubmitted uid
  list <- mapM (exerciseReport uid base) exs
  return (doc $
           header 1 "Relatório de submissão" <>
           para (text "Nome:" <> text (fullname)) <>
           para (text "Login:" <> text (show uid)) <>
           mconcat list)


exerciseReport :: UserLogin -> FilePath -> FilePath -> Codex Blocks
exerciseReport uid base expath = do
  page <- liftIO $ readMarkdownFile (base </> expath)
  optSub <- getLast uid expath
  let title = maybe (text "Untitled") fromList (pageTitle page)
  return (header 1 title <>
          maybe (para "Nenhuma solução submetida.") report optSub
         )
  where
    report Submission{..}
      = header 2 "Resultado" <>
        para (emph $ text $ show $ resultClassify submitResult) <>
        header 2 (text "Submissão enviada") <>
        codeBlock (T.unpack $ codeText submitCode) <>
        header 2 (text "Relatório detalhado") <>
        codeBlock (T.unpack $ resultMessage submitResult)


getLast uid ex = do
  s1 <- getLastAccepted uid ex
  s2 <- getLastSubmitted uid ex
  return (s1 <|> s2)
