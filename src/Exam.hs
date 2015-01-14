{-
-- quick hack script to set problems start and end times for exams
-}
module Main where

import System.Environment
import System.Locale
import System.FilePath
import System.IO
import Data.Time.Clock
import Data.Time.LocalTime
import Data.Time.Format

main :: IO ()
main = getArgs >>= runArgs


runArgs :: [String] -> IO ()
runArgs ("stop":args)= mapM_ (append txt) args
    where txt = unlines [tagged "open" (tagged "empty" "")]

runArgs ("start":arg1:args)
        = do tz <- getCurrentTimeZone
             t0 <- getCurrentTime         -- start time
             let duration = read arg1 :: Int        -- seconds
             let t1 = addUTCTime (fromIntegral duration) t0  -- end time
             let txt = unlines [
                         tagged "open" $ 
                         unlines ["",
                                  tagged "start" (fmtTime $ utcToLocalTime tz t0),
                                  tagged "end"  (fmtTime $ utcToLocalTime tz t1)
                                 ]
                        ]
             mapM_ (append txt) args
runArgs args  = hPutStrLn stderr ("invalid args: " ++ unwords args)


tagged tag str = "<" ++ tag ++">" ++ str ++ "</" ++ tag ++ ">"

fmtTime  = formatTime defaultTimeLocale "%H:%M %d/%m/%Y" 

append :: String -> FilePath -> IO ()
append text path
    | takeExtension path == ".xml" = appendFile path text
    | otherwise =  hPutStrLn stderr ("ignoring non HTML file: "++ path)
