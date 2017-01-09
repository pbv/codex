{-# LANGUAGE OverloadedStrings #-}
module Codex.Config where

import           Codex.Types
import           Codex.SafeExec

import           Data.Maybe (fromMaybe)
import qualified Data.Configurator as Configurator
import           Data.Configurator.Types
import qualified Data.HashMap.Strict as HashMap
import           Snap.Util.FileServe(MimeMap, defaultMimeTypes)

import           Control.Concurrent.QSem

-- | configured semaphore for throttling evaluation t
getEvalQS :: Name -> Config -> IO QSem
getEvalQS prefix conf = do
  mbN <- Configurator.lookup conf prefix
  newQSem (fromMaybe 10 mbN)  -- default value


getSafeExecConf :: Config -> IO SafeExecConf
getSafeExecConf conf = do
  path <- Configurator.lookup conf "path"
  cpu  <- Configurator.lookup conf "max_cpu"
  clock<- Configurator.lookup conf "max_clock"
  mem  <- Configurator.lookup conf "max_memory"
  stack <- Configurator.lookup conf"max_stack"
  nproc<- Configurator.lookup conf "num_proc"
  return SafeExecConf { safeExecPath = path
             , maxCpuTime   = cpu
             , maxClockTime = clock
             , maxMemory    = mem
             , maxStack     = stack
             , numProc      = nproc
             , maxFSize     = Nothing
             , maxCore      = Nothing
             }

{-
getPrintConf :: Config -> IO PrintConf
getPrintConf conf = do
  enabled <- Configurator.lookupDefault False conf "printouts.enabled"
  header <- Configurator.lookupDefault defaultHeader conf "printouts.header"
  opts <- Configurator.lookupDefault [] conf "printouts.options"
  return (PrintConf enabled header opts)
  where defaultHeader = "Codex"
-}


getLdapConf ::  Config -> IO (Maybe LdapConf)
getLdapConf conf = do
  enabled <- Configurator.lookupDefault False conf "enabled"
  if enabled then do uri <- Configurator.require conf "uri"
                     base <- Configurator.require conf "base"
                     assocs <- Configurator.require conf "attrs"
                     let attrs = HashMap.fromList assocs
                     return (Just (LdapConf uri base attrs))
    else return Nothing



-- | constant paths
publicPath :: FilePath
publicPath = "public"

staticPath :: FilePath
staticPath = "static"

-- | custom mime type mapping
mimeTypes :: MimeMap
mimeTypes
  = HashMap.union defaultMimeTypes $
    HashMap.fromList [(".tst", "text/plain"),
                      (".py",  "text/plain"),
                      (".mdown", "text/markdown"),
                      (".md",  "text/markdown"),
                      (".db", "application/x-sqlite3")]
