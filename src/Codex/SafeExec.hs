{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-
  Run UNIX commands under a "sandbox" limiting memory, cpu time, etc.
-}
module Codex.SafeExec (
  SafeExecConf(..),
  safeExecConfig,
  safeExecWith
  ) where

import           Data.Text (Text)
import           Data.Maybe

import           System.Exit (ExitCode)
import           System.Process.Text (readProcessWithExitCode)
import           Data.Configurator.Types(Config)
import qualified Data.Configurator as Configurator

import           Data.Monoid
import           Control.Monad

  

-- | SafeExec configuration parameters
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


instance Monoid SafeExecConf where
  -- | empty configuration
  mempty =  SafeExecConf { safeExecPath = Nothing
                         , maxCpuTime   = Nothing
                         , maxClockTime = Nothing
                         , maxMemory    = Nothing
                         , maxStack     = Nothing
                         , numProc      = Nothing
                         , maxFSize     = Nothing
                         , maxCore      = Nothing
                         }            
  -- | combine configurations, overriding rhs with lhs 
  l `mappend` r = SafeExecConf {
    safeExecPath = safeExecPath l `mplus` safeExecPath r,
    maxCpuTime   = maxCpuTime l `mplus` maxCpuTime r,
    maxClockTime = maxClockTime l `mplus` maxClockTime r,    
    maxMemory    = maxMemory l `mplus` maxMemory r,
    maxStack     = maxStack l `mplus` maxStack r,
    maxFSize     = maxFSize l `mplus` maxFSize r,
    maxCore      = maxCore l `mplus` maxCore r,
    numProc      = numProc l `mplus` numProc r
    }



-- | lookup a SafeExec configuration from a Config value
safeExecConfig :: Config -> IO SafeExecConf
safeExecConfig conf = do
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


-- | run a command under SafeExec
safeExecWith :: SafeExecConf
             -> FilePath   -- ^ command
             -> [String]   -- ^ arguments
             -> Text       -- ^ stdin
             -> IO (ExitCode, Text, Text)  -- exitcode, stdout, stderr

safeExecWith SafeExecConf{..} cmd args stdin
  = let mkArg opt = maybe [] (\c -> [opt, show c])
        args0 = mkArg "--cpu" maxCpuTime
                ++
                mkArg "--clock" maxClockTime
                ++
                mkArg "--mem" maxMemory
                ++
                mkArg "--stack" maxStack
                ++
                mkArg "--fsize" maxFSize
                ++
                mkArg "--core" maxCore
                ++
                mkArg "--nproc" numProc
                ++
                ["--exec", cmd]
    in do
     readProcessWithExitCode
       (fromMaybe "safeexec" safeExecPath) (args0 ++ args) stdin



