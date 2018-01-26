{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-
  Run UNIX commands under a "sandbox" limiting memory, cpu time, etc.
-}
module Codex.SafeExec (
  Limits(..),
  getLimits,
  safeExecWith
  ) where

import           Data.Text (Text)

import           System.Exit (ExitCode)
import           System.Process.Text (readProcessWithExitCode)
import           Data.Configurator.Types(Config)
import qualified Data.Configurator as Configurator

import           Control.Monad (mplus)

  

-- | SafeExec limits 
data Limits =  Limits { maxCpuTime :: !(Maybe Int)   -- seconds
                      , maxClockTime :: !(Maybe Int) -- seconds
                      , maxMemory :: !(Maybe Int)    -- KB
                      , maxStack :: !(Maybe Int)     -- KB
                      , maxFSize :: !(Maybe Int)     -- KB
                      , maxCore :: !(Maybe Int)      -- KB
                      , numProc :: !(Maybe Int)
                      } deriving (Eq, Show, Read)


instance Monoid Limits where
  -- | empty element; no limits set
  mempty =  Limits { maxCpuTime   = Nothing
                   , maxClockTime = Nothing
                   , maxMemory    = Nothing
                   , maxStack     = Nothing
                   , numProc      = Nothing
                   , maxFSize     = Nothing
                   , maxCore      = Nothing
                   }            
  -- | combine limits, overriding rhs with lhs 
  l `mappend` r
    = Limits { maxCpuTime   = maxCpuTime l `mplus` maxCpuTime r,
               maxClockTime = maxClockTime l `mplus` maxClockTime r,    
               maxMemory    = maxMemory l `mplus` maxMemory r,
               maxStack     = maxStack l `mplus` maxStack r,
               maxFSize     = maxFSize l `mplus` maxFSize r,
               maxCore      = maxCore l `mplus` maxCore r,
               numProc      = numProc l `mplus` numProc r
             }



-- | lookup limits from config values
getLimits :: Config -> IO Limits
getLimits cfg = do
  cpu  <- Configurator.lookup cfg "max_cpu"
  clock<- Configurator.lookup cfg "max_clock"
  mem  <- Configurator.lookup cfg "max_memory"
  stack <- Configurator.lookup cfg"max_stack"
  nproc<- Configurator.lookup cfg "num_proc"
  max_fsize <- Configurator.lookup cfg "max_fsize"
  max_core <- Configurator.lookup cfg "max_core"
  return Limits { maxCpuTime   = cpu
                , maxClockTime = clock
                , maxMemory    = mem
                , maxStack     = stack
                , numProc      = nproc
                , maxFSize     = max_fsize
                , maxCore      = max_core
                }


-- | run a command under SafeExec
safeExecWith :: FilePath   -- ^ safe exec path
             -> Limits
             -> FilePath   -- ^ command
             -> [String]   -- ^ arguments
             -> Text       -- ^ stdin
             -> IO (ExitCode, Text, Text)  -- exitcode, stdout, stderr

safeExecWith safeExecPath Limits{..} cmd args stdin
  = let mkArg opt = maybe [] (\c -> [opt, show c])
        args' = mkArg "--cpu" maxCpuTime
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
                ["--exec", cmd] ++
                args
    in do
     readProcessWithExitCode safeExecPath args' stdin



