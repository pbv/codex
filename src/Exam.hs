--
-- quick hack script to set problems start and end times for exams
--
module Main where

import System.Environment
import System.Locale
import System.FilePath
import System.IO
import Data.Time.Clock
import Data.Time.LocalTime
import Data.Time.Format

import Problem



main :: IO ()
main = do argv <- getArgs
          tz <- getCurrentTimeZone
          t0 <- getCurrentTime         -- start time
          let t1 = addUTCTime 3600 t0  -- plus 1h
          let xtra = unlines ["", tagged "start" (utcToLocalTime tz t0),
                              tagged "end"  (utcToLocalTime tz t1), ""]
          mapM_ (append xtra) argv
  
  where append xtra path
          | takeExtension path == ".html" = appendFile path xtra
          | otherwise =  hPutStrLn stderr ("ignoring non HTML file: "++ path)


tagged  :: String -> LocalTime -> String
tagged tag time = "<" ++ tag ++">" ++ fmt ++ "</" ++ tag ++ ">"
  where fmt = formatTime defaultTimeLocale (head dateFormats) time
