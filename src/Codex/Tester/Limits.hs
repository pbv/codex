{-# LANGUAGE OverloadedStrings #-}
{-
  Limits for memory, cpu time, etc. under a "sandbox" 
-}
module Codex.Tester.Limits (
  Limits(..),
  configLimits,
  ) where


import           Data.Configurator.Types(Config)
import qualified Data.Configurator as Configurator

import           Control.Monad (mplus)


-- | SafeExec limits 
data Limits
  = Limits { maxCpuTime :: !(Maybe Int)   -- seconds
           , maxClockTime :: !(Maybe Int) -- seconds
           , maxMemory :: !(Maybe Int)    -- KB
           , maxStack :: !(Maybe Int)     -- KB
           , maxFSize :: !(Maybe Int)     -- KB
           , maxCore :: !(Maybe Int)      -- KB
           , numProc :: !(Maybe Int)
           } deriving (Eq, Show, Read)


instance Monoid Limits where
  -- | empty element; no limits set
  mempty 
    = Limits { maxCpuTime   = Nothing
             , maxClockTime = Nothing
             , maxMemory    = Nothing
             , maxStack     = Nothing
             , numProc      = Nothing
             , maxFSize     = Nothing
             , maxCore      = Nothing
             }            

instance Semigroup Limits where
  -- | combine limits; left-hand side overrriding right-hand side
  l <> r
    = Limits { maxCpuTime   = maxCpuTime l `mplus` maxCpuTime r,
               maxClockTime = maxClockTime l `mplus` maxClockTime r,    
               maxMemory    = maxMemory l `mplus` maxMemory r,
               maxStack     = maxStack l `mplus` maxStack r,
               maxFSize     = maxFSize l `mplus` maxFSize r,
               maxCore      = maxCore l `mplus` maxCore r,
               numProc      = numProc l `mplus` numProc r
             }



-- | lookup limits from a subconfig 
configLimits :: Config -> IO Limits
configLimits cfg = do
  cpu  <- Configurator.lookup cfg "max_cpu"
  clock<- Configurator.lookup cfg "max_clock"
  mem  <- Configurator.lookup cfg "max_memory"
  stack <- Configurator.lookup cfg "max_stack"
  nproc <- Configurator.lookup cfg "num_proc"
  max_fsize <- Configurator.lookup cfg "max_fsize"
  max_core <- Configurator.lookup cfg "max_core"
  return Limits  { maxCpuTime   = cpu
                 , maxClockTime = clock
                 , maxMemory    = mem
                 , maxStack     = stack
                 , numProc      = nproc
                 , maxFSize     = max_fsize
                 , maxCore      = max_core
                 }




