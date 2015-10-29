{-# LANGUAGE OverloadedStrings, RecordWildCards #-} 
{-
   Produce printouts for exams, etc.
-}
module Printout
       (makePrintout
       ) where
-- import           Prelude hiding (catch)
import           System.FilePath
import           System.Directory
import           System.Process
import           System.Locale(defaultTimeLocale)
import           System.IO.Error

import           Data.Time.Clock
import           Data.Time.LocalTime
import           Data.Time.Format

import qualified Data.ByteString.Char8 as B
import           Data.ByteString.Char8 (ByteString)
import           Data.Text(Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
-- import           Data.Maybe

-- import           Control.Applicative
import           Control.Monad (forM_)

-- import           Snap.Core
-- import           Application
-- import           Utils
-- import           Types
-- import           Problem
-- import           Submission




  
{-
dnsLookup :: String -> IO String
dnsLookup addr = 
  catchIOError (readProcess "/usr/bin/dig" ["@192.168.0.2", "+short", "-x", addr] "")
          (\_ -> return "")
-}

        
makePrintout :: FilePath -> Text -> IO () 
makePrintout prefix report = do
  -- LaTeX and PDF file paths 
  let texfile = prefix <.> "tex"
  let pdffile = prefix <.> "pdf"
  -- write LaTeX report file 
  T.writeFile (printoutDir</>texfile) report
  -- run pdflatex 
  (_,_,_,ph) <- createProcess (pdflatex texfile) {cwd=Just printoutDir}
  waitForProcess ph                    -- wait for it
  -- remove temporary files
  let tmpfiles = map (printoutDir</>) [prefix<.>"aux", prefix<.>"log"]
  forM_ tmpfiles $ \f -> catchIOError (removeFile f) (\_ -> return ()) 
  
{-
makePrintout Printout{..} uid name client subs = do
  zonetime <- getCurrentTime >>= utcToLocalZonedTime
  let time = T.pack (formatTime defaultTimeLocale "%c" zonetime)
  -- LaTeX and PDF file paths 
  let texfile = show uid <.> "tex"
  let auxfile = show uid <.> "aux"
  let logfile = show uid <.> "log"  
  let pdffile = show uid <.> "pdf"
  -- write LaTeX source
  T.writeFile ("printouts"</>texfile) (genLaTeX uid printHeader name client time subs)
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
-}            
            
pdflatex :: FilePath -> CreateProcess
pdflatex file = proc "pdflatex" ["-interaction=batchmode", file]

{-
genLaTeX :: UID        -- user ID
            -> Text    -- header
            -> Text    -- full name
            -> Text    -- client IP
            -> Text    -- current time
            -> [(Problem, Maybe Submission)] 
            -- problems and final submission
            -> Text                            -- LaTeX source
genLaTeX uid header name client time subs
  = T.concat (preamble uid header name client time ++
              concatMap submission subs ++
              closing)

submission (Problem{..}, Nothing) = 
  ["\\section*{", title, "}\n",
   "(Nenhuma solução submetida.)\n"
  ]
  where title = maybe "N/A" id probTitle
submission (Problem{..}, Just Submission{..}) =
  ["\\section*{", title, "}\n",
   "\\textbf{Resultado:} ", result, "\n",
   "\\begin{Verbatim}[frame=lines,numbers=left]\n",
   T.strip submitText, "\n",
   "\\end{Verbatim}\n"
  ] ++
  if ok then [] else
  [ "\\begin{Verbatim}[frame=leftline,fontshape=sl]\n",
    T.strip submitReport, "\n",
    "\\end{Verbatim}\n" ]
  where title = maybe "N/A" id probTitle
        ok = submitStatus == Accepted || submitStatus == Overdue
        result
          | submitStatus == Accepted = "Passou todos os testes."
          | submitStatus == Overdue  = "Submitido fora do tempo."
          | otherwise                = "Falhou algum(s) teste(s)."

-}


printoutDir :: FilePath
printoutDir = "printouts"




