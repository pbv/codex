{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-
  Run UNIX commands under a "sandbox" limiting memory, cpu time, etc.
-}
module Codex.SafeExec where

import           Data.Text (Text)
import           Data.Maybe

import           System.Exit (ExitCode)
import           System.Process.Text (readProcessWithExitCode)
import           Data.Configurator.Types(Config)
import qualified Data.Configurator as Configurator

import           Control.Applicative
  

-- | safeexec configuration parameters
data SafeExecConf =
  SafeExecConf { safeExecPath :: Maybe FilePath
               , maxCpuTime :: Maybe Int   -- seconds
               , maxClockTime :: Maybe Int -- seconds
               , maxMemory :: Maybe Int    -- KB
               , maxStack :: Maybe Int     -- KB
               , maxFSize :: Maybe Int     -- KB
               , maxCore :: Maybe Int      -- KB
               , numProc :: Maybe Int
               } deriving (Eq, Show, Read)


getSafeExecConf :: Config -> IO SafeExecConf
getSafeExecConf conf = do
  path <- Configurator.lookup conf "path"
  cpu  <- Configurator.lookup conf "max_cpu"
  clock<- Configurator.lookup conf "max_clock"
  mem  <- Configurator.lookup conf "max_memory"
  stack <- Configurator.lookup conf"max_stack"
  nproc<- Configurator.lookup conf "num_proc"
  max_fsize <- Configurator.lookup conf "max_fsize"
  max_core <- Configurator.lookup conf "max_core"
  return SafeExecConf { safeExecPath = path
                      , maxCpuTime   = cpu
                      , maxClockTime = clock
                      , maxMemory    = mem
                      , maxStack     = stack
                      , numProc      = nproc
                      , maxFSize     = max_fsize
                      , maxCore      = max_core
                      }


-- | combine two configs, overriding rhs settings with lhs ones
override :: SafeExecConf -> SafeExecConf -> SafeExecConf
c1 `override` c2 = SafeExecConf {
    safeExecPath = safeExecPath c1 <|> safeExecPath c2,
    maxCpuTime   = maxCpuTime c1 <|> maxCpuTime c2,
    maxClockTime = maxClockTime c1 <|> maxClockTime c2,    
    maxMemory    = maxMemory c1 <|> maxMemory c2,
    maxStack     = maxStack c1 <|> maxStack c2,
    maxFSize     = maxFSize c1 <|> maxFSize c2,
    maxCore      = maxCore c1 <|> maxCore c2,
    numProc      = numProc c1 <|> numProc c2
    }





-- | run with optional safeexec
safeExecWith :: SafeExecConf
             -> FilePath   -- ^ command
             -> [String]   -- ^ arguments
             -> Text       -- ^ stdin
             -> IO (ExitCode, Text, Text)  -- exitcode, stdout, stderr

safeExecWith SafeExecConf{..} cmd args stdin
  = let arg opt = maybe [] (\c -> [opt, show c])
        args0 = arg "--cpu" maxCpuTime
                ++
                arg "--clock" maxClockTime
                ++
                arg "--mem" maxMemory
                ++
                arg "--stack" maxStack
                ++
                arg "--fsize" maxFSize
                ++
                arg "--core" maxCore
                ++
                arg "--nproc" numProc
                ++
                ["--exec", cmd]
    in do
     readProcessWithExitCode
       (fromMaybe "safeexec" safeExecPath) (args0 ++ args) stdin



