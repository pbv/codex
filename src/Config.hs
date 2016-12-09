{-# LANGUAGE OverloadedStrings #-}
module Config where

import           Types
import           SafeExec
import           Control.Applicative
import           Data.Monoid
import qualified Data.Configurator as Configurator
import           Data.Configurator.Types
import qualified Data.HashMap.Strict as HashMap
import           Snap.Util.FileServe(MimeMap, defaultMimeTypes)


getSafeExecConf :: Name -> Config -> IO SafeExecConf
getSafeExecConf prefix conf = do
  path <- Configurator.lookup conf (prefix <> ".path")
  cpu  <- Configurator.lookup conf (prefix <> ".max_cpu")
  clock<- Configurator.lookup conf (prefix <> ".max_clock")
  mem  <- Configurator.lookup conf (prefix <> ".max_memory")
  stack <- Configurator.lookup conf (prefix <> ".max_stack")
  nproc<- Configurator.lookup conf (prefix <> ".num_proc")
  return
      mempty { safeExecPath = path
             , maxCpuTime   = cpu 
             , maxClockTime = clock
             , maxMemory    = mem
             , maxStack     = stack
             , numProc      = nproc
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


getLdapConf ::  Name -> Config -> IO (Maybe LdapConf)
getLdapConf prefix conf = do
  enabled <- Configurator.lookupDefault False conf (prefix <> ".enabled")
  if enabled then do uri <- Configurator.require conf (prefix <> ".uri")
                     base <- Configurator.require conf (prefix <> ".base")
                     assocs <- Configurator.require conf (prefix <> ".attrs")
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


