{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
{- 
  Generate a LaTeX report for exam printout, etc.
  Pedro Vasconcelos, 2015
-}
module Report (genReport) where

import           Control.Monad.IO.Class (liftIO)
import           Control.Applicative
import           Data.Text(Text)
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as B
import           Data.Monoid
import           System.Locale(defaultTimeLocale)

import           Data.Time.Clock
import           Data.Time.LocalTime
import           Data.Time.Format

import           Snap.Core
import           Application
import           Utils
import           Types
import           Problem
import           Submission


genReport ::  [(Problem, Maybe Submission)] -> Pythondo Text
genReport subs = do
  zonetime <- liftIO (getCurrentTime >>= utcToLocalZonedTime)
  let time = T.pack (formatTime defaultTimeLocale "%c" zonetime)
  uid <- require getUserID
  fullname <- require getFullName
  clientAddr <- getClientAddr
  header <- printHeader <$> getPrintConf
  let preamble = makePreamble uid header fullname clientAddr time
  let blocks = mconcat (map reportSubmission subs)
  return (preamble <> blocks <> closing)



reportSubmission :: (Problem, Maybe Submission) -> Text
reportSubmission (p@Problem{..}, Nothing) 
    = section_ (getTitle p) <> "(Nenhuma solução submetida.)\n"
reportSubmission (p@Problem{..}, Just Submission{..})   
    = section_ (getTitle p) <>
      paragraph "Resultado: " <> result <> "\n" <>
      verbatimCode (T.strip submitText) <>
      if ok then "" else verbatimMsgs (T.strip submitReport)
    where result = case submitStatus of
              Accepted -> "Passou todo os testes."
              Overdue -> "Submitido fora do tempo."
              _ -> "Falhou algum(s) teste(s)."
          ok = submitStatus==Accepted || submitStatus==Overdue

getTitle :: Problem -> Text
getTitle Problem{..} =  maybe "Untitled" id probTitle

-- fetch remote client address (maybe forwarded by a proxy)
getClientAddr :: Pythondo Text
getClientAddr = do
  addr <- getsRequest rqRemoteAddr
  -- clientname <- liftIO $ dnsLookup (B.unpack addr)
  return (T.pack $ B.unpack addr)



makePreamble uid header name client time 
   = T.concat 
     ["\\documentclass[10pt,a4paper,twoside]{article}\n",
      "\\usepackage[T1]{fontenc}\n",
      "\\usepackage[utf8]{inputenc}\n",
      "\\usepackage{fancyvrb}\n",
      "\\usepackage{fancyhdr}\n",
      "\\addtolength{\\oddsidemargin}{-.875in}\n",
      "\\addtolength{\\evensidemargin}{-.875in}\n",
      "\\addtolength{\\textwidth}{1.75in}\n",
      "\\addtolength{\\topmargin}{-.875in}\n",
      "\\addtolength{\\textheight}{1.75in}\n",
      "\\pagestyle{fancy}\n",
      "\\lhead{", name, "}\n",
      "\\rhead{\\texttt{", T.pack (show uid), "}}\n",
      "\\cfoot{\\thepage}\n",
      "\\begin{document}\n",
      "\\thispagestyle{plain}\n",
      "\\noindent\\parbox{\\textwidth}{", header, "\\\\[1ex]\n",
      "\\textbf{Data:} ", time, "\\\\[2ex]\n",
      "\\textbf{Nome:} ", name, "\\\\[2ex]\n",
      "\\textbf{Login:} \\texttt{", T.pack (show uid), "@", client, "} \\\\[2ex]\n",
      "\\textbf{Assinatura:} \\hrulefill}\\bigskip\n" 
     ]
   
closing = "\\end{document}\n"


section_ :: Text -> Text
section_ title  
    = T.concat ["\\section*{", title, "}\n"]

paragraph :: Text -> Text 
paragraph title = T.concat ["\\paragraph{", title, "}"]

verbatimCode :: Text -> Text
verbatimCode arg 
    = T.concat ["\\begin{Verbatim}[frame=lines,numbers=left]\n", arg, 
                "\n\\end{Verbatim}\n"]

verbatimMsgs :: Text -> Text
verbatimMsgs arg 
    = T.concat ["\\begin{Verbatim}[frame=leftline,fontshape=sl]\n", arg, 
                "\n\\end{Verbatim}\n"]
