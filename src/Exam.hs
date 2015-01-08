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

--import Problem


duration = 3600      -- exam duration (secs)


main :: IO ()
main = do argv <- getArgs
          tz <- getCurrentTimeZone
          t0 <- getCurrentTime         -- start time
          let t1 = addUTCTime duration t0  -- end time
          let xtra = unlines [
                      tagged "open" $ 
                      unlines ["",
                               tagged "start" (fmtTime $ utcToLocalTime tz t0),
                               tagged "end"  (fmtTime $ utcToLocalTime tz t1)
                              ]
                      ]
          mapM_ (append xtra) argv
  
  where append xtra path
          | takeExtension path == ".xml" = 
              -- appendFile path xtra
              do readFile path >>= putStrLn
                 putStrLn xtra
          | otherwise =  hPutStrLn stderr ("ignoring non HTML file: "++ path)


tagged tag str = "<" ++ tag ++">" ++ str ++ "</" ++ tag ++ ">"

fmtTime  = formatTime defaultTimeLocale "%H:%M %d/%m/%Y" 
