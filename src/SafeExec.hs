{-
  Run UNIX commands under a "sandbox" limiting memory, cpu time, etc.
-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module SafeExec where

import           Data.Text (Text)
import           Data.Monoid
import           Data.Maybe

import           System.Exit (ExitCode)
import           System.Process.Text (readProcessWithExitCode)

import           Control.Monad
import           Control.Applicative
-- import           Test.QuickCheck


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

instance Monoid SafeExecConf where
  mempty = SafeExecConf {
    safeExecPath = Nothing,
    maxCpuTime   = Nothing,
    maxClockTime = Nothing,
    maxMemory    = Nothing,
    maxStack     = Nothing,
    maxFSize     = Nothing,
    maxCore      = Nothing,
    numProc      = Nothing
    }

  mappend c1 c2 = SafeExecConf {
    safeExecPath = safeExecPath c1 `mplus` safeExecPath c2,
    maxCpuTime   = maxCpuTime c1 `mplus` maxCpuTime c2,
    maxClockTime = maxClockTime c1 `mplus` maxClockTime c2,    
    maxMemory    = maxMemory c1 `mplus` maxMemory c2,
    maxStack     = maxStack c1 `mplus` maxStack c2,
    maxFSize     = maxFSize c1 `mplus` maxFSize c2,
    maxCore      = maxCore c1 `mplus` maxCore c2,
    numProc      = numProc c1 `mplus` numProc c2
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




{-
instance Arbitrary SafeExecConf where
  arbitrary = SafeExecConf <$>
              arbitrary <*>
              arbitrary <*>
              arbitrary <*>
              arbitrary <*>
              arbitrary <*>
              arbitrary <*>
              arbitrary <*>
              arbitrary 

prop_neutral x = mappend x mempty == x &&
                 mappend mempty x == x
  where types = x :: SafeExecConf

prop_assoc x y z = mappend x (mappend y z) ==
                    mappend (mappend x y) z
  where types = x :: SafeExecConf
-}                    

