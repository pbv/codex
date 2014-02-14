{-# LANGUAGE OverloadedStrings, RecordWildCards #-} 
{-
   Produce printouts for exams, etc.
-}
module Printout(handlePrintout) where
import           Prelude hiding (catch)
import           System.FilePath
import           System.Directory
import           System.Process
import           System.Locale(defaultTimeLocale)

import           Data.Time.Clock
import           Data.Time.LocalTime
import           Data.Time.Format

import           Data.Text(Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Data.Maybe

import           Data.Configurator
import           Data.Configurator.Types

import           Control.Monad.State

import           Application
import           Utils
import           Types
import           Problem
import           Submission




-- | handle printout when a user logs out
handlePrintout :: UID -> AppHandler ()
handlePrintout uid = do 
  printout <- getPrintout  -- printout configuration
  fullname <- getFullName
  probs <- liftIO getProblems
  subreps <- sequence [getFinalReport uid prob | prob<-probs]
  let psubs = [(p,s,r) | (p, Just (s,r))<-zip probs subreps]
  liftIO $ makePrintout printout uid fullname psubs


makePrintout :: Printout -> UID -> Text 
                -> [(Problem UTCTime, Submission, Report)] 
                -> IO () 
makePrintout Printout{..} uid name psubs = do
  zonetime <- getCurrentTime >>= utcToLocalZonedTime
  let time = T.pack (formatTime defaultTimeLocale "%c" zonetime)
  -- LaTeX and PDF file paths 
  let texfile = show uid <.> "tex"
  let auxfile = show uid <.> "aux"
  let logfile = show uid <.> "log"  
  let pdffile = show uid <.> "pdf"
  -- write LaTeX source
  T.writeFile ("printouts"</>texfile) (genLaTeX uid printHeader name time psubs)
  -- run pdflatex 
  (_,_,_,ph) <- createProcess (pdflatex texfile) {cwd=Just "printouts"}
  waitForProcess ph                    -- wait for it
  removeFile ("printouts"</>auxfile)   -- remove temporary files
  removeFile ("printouts"</>logfile)
  -- run lpr (if enabled)
  when printEnabled $ do  
    let cmd = proc "lpr" (printOptions ++ [pdffile]) 
    (_,_,_,ph) <- createProcess cmd {cwd=Just "printouts"}
    waitForProcess ph
    return ()
            
            
pdflatex :: FilePath -> CreateProcess
pdflatex file = proc "pdflatex" ["-interaction=batchmode", file]


genLaTeX :: UID        -- user ID
            -> Text    -- header
            -> Text    -- full name
            -> Text    -- current time
            -> [(Problem UTCTime, Submission, Report)] -- problems and final submission
            -> Text                                    -- LaTeX report
genLaTeX uid header name time psubs
  = T.concat (preamble uid header name time ++
              concatMap submission psubs ++
              closing)

submission (prob,sub,rep) =
  ["\\section*{", probTitle prob, "}\n",
   "\\textbf{Resultado:} ", result, "\n",
   "\\begin{Verbatim}[frame=lines,numbers=left]\n",
   T.strip (submitText sub), "\n",
   "\\end{Verbatim}\n"
  ] ++
  if ok then [] else
  [ "\\begin{Verbatim}[frame=leftline,fontshape=sl]\n",
    T.strip (reportStdout rep), "\n",
    T.strip (reportStderr rep), "\n",
    "\\end{Verbatim}\n" ]
  where status = reportStatus rep
        ok = status == Accepted || status == Overdue
        result
          | status == Accepted = "Passou todos os testes."
          | status == Overdue  = "Submitido fora do tempo."
          | otherwise          = "Falhou algum(s) teste(s)."

preamble uid header name time = 
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
    "\\textbf{Nome:} ", name, 
    "\\hfill\\textbf{Login:} \\texttt{", T.pack (show uid), "} \\\\[2ex]\n",
    "\\textbf{Assinatura:} \\hrulefill}\\bigskip\n" 
   ]
   
closing = ["\\end{document}\n"]


             