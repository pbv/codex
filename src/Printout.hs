{-# LANGUAGE OverloadedStrings, RecordWildCards #-} 
{-
   Produce printouts for exams, etc.
-}
module Printout
       (handlePrintout
       ) where
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




-- | make a printout before ending session
handlePrintout :: UID -> AppHandler ()
handlePrintout uid = do 
  printout <- getPrintout  -- printout configuration
  fullname <- getFullName
  pids <- getSubmittedPIDs uid
  subs <- mapM (getBestSubmission uid) pids
  probs <-liftIO $ mapM getProblem pids
  liftIO $ makePrintout printout uid fullname (zip probs subs)

        
makePrintout :: Printout 
                -> UID 
                -> Text 
                -> [(Problem UTCTime, Maybe Submission)] 
                -> IO () 
makePrintout Printout{..} uid name subs = do
  zonetime <- getCurrentTime >>= utcToLocalZonedTime
  let time = T.pack (formatTime defaultTimeLocale "%c" zonetime)
  -- LaTeX and PDF file paths 
  let texfile = show uid <.> "tex"
  let auxfile = show uid <.> "aux"
  let logfile = show uid <.> "log"  
  let pdffile = show uid <.> "pdf"
  -- write LaTeX source
  T.writeFile ("printouts"</>texfile) (genLaTeX uid printHeader name time subs)
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
            -> [(Problem UTCTime, Maybe Submission)] 
            -- problems and final submission
            -> Text                            -- LaTeX source
genLaTeX uid header name time subs
  = T.concat (preamble uid header name time ++
              concatMap submission subs ++
              closing)

submission (Problem{..}, Nothing) = 
  ["\\section*{", probTitle, "}\n",
   "(Nenhuma solução submetida.)\n"
  ]
submission (Problem{..}, Just Submission{..}) =
  ["\\section*{", probTitle, "}\n",
   "\\textbf{Resultado:} ", result, "\n",
   "\\begin{Verbatim}[frame=lines,numbers=left]\n",
   T.strip submitText, "\n",
   "\\end{Verbatim}\n"
  ] ++
  if ok then [] else
  [ "\\begin{Verbatim}[frame=leftline,fontshape=sl]\n",
    T.strip submitReport, "\n",
    "\\end{Verbatim}\n" ]
  where ok = submitStatus == Accepted || submitStatus == Overdue
        result
          | submitStatus == Accepted = "Passou todos os testes."
          | submitStatus == Overdue  = "Submitido fora do tempo."
          | otherwise                = "Falhou algum(s) teste(s)."

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


