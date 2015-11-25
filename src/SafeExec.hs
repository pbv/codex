{-
  Run UNIX commands under a "sandbox" limiting memory, cpu time, etc.
-}
{-# LANGUAGE RecordWildCards, OverloadedStrings #-}
module SafeExec where

import qualified Data.Text as T
import           Data.Text (Text)

import System.Exit (ExitCode)
import System.Process.Text (readProcessWithExitCode)
-- import System.Process(readProcessWithExitCode)

-- | safeexec configuration parameters
data SafeExecConf =
  SafeExecConf { safeExecPath :: !FilePath
               , maxCpuTime :: !Int   -- seconds
               , maxClockTime :: !Int -- seconds
               , maxMemory :: !Int    -- KB
               , maxStack :: !Int     -- KB
               , maxFSize :: !Int     -- KB
               , maxCore :: !Int      -- KB
               , maxProc :: !Int
               } deriving (Eq, Show)


-- | default parameters
defaultConf :: SafeExecConf
defaultConf = SafeExecConf { safeExecPath = "safeexec"
                            , maxCpuTime = 2
                            , maxClockTime = 15
                            , maxMemory= 10*1024
                            , maxStack = 8*1024
                            , maxFSize = 8*1024
                            , maxCore = 0
                            , maxProc = 0
                            }


safeExecWith :: SafeExecConf
                -> FilePath   -- ^ command
                -> [String]   -- ^ arguments
                -> Text       -- ^ stdin
                -> IO (ExitCode, Text, Text)  -- exitcode, stdout, stderr
safeExecWith SafeExecConf{..} cmd args stdin
  = readProcessWithExitCode safeExecPath (args0 ++ args) stdin
  where args0 = ["--cpu", show maxCpuTime,
                 "--clock", show maxClockTime,
                 "--mem", show maxMemory,
                 "--stack", show maxStack,
                 "--fsize", show maxFSize,
                 "--core", show maxCore,
                 "--nproc", show maxProc,
                 "--exec", cmd]
                   

safeExec :: FilePath -> [String] -> Text -> IO (ExitCode,Text,Text)
safeExec = safeExecWith defaultConf



