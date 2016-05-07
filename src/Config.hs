{-# LANGUAGE OverloadedStrings #-}
module Config where

import           Types
import           SafeExec
import           Data.Maybe (fromMaybe)

import           Data.Monoid
import qualified Data.Configurator as Configurator
import           Data.Configurator.Types



getSafeExecConf :: Name -> Config -> IO SafeExecConf
getSafeExecConf prefix conf = do
  path <- Configurator.lookup conf (prefix <> "safeexec.path")
  cpu  <- Configurator.lookup conf (prefix <> "safeexec.max_cpu")
  clock<- Configurator.lookup conf (prefix <> "safeexec.max_clock")
  mem  <- Configurator.lookup conf (prefix <> "safeexec.max_memory")
  nproc<- Configurator.lookup conf (prefix <> "safeexec.num_proc")
  let def = defaultSafeExecConf
  return
      def { safeExecPath = fromMaybe (safeExecPath def) path
                , maxCpuTime   = fromMaybe (maxCpuTime def) cpu 
                , maxClockTime = fromMaybe (maxClockTime def) clock
                , maxMemory    = fromMaybe (maxMemory def)  mem
                , numProc      = fromMaybe (numProc def) nproc
                }


getPrintConf :: Config -> IO PrintConf
getPrintConf conf = do
  enabled <- Configurator.lookupDefault False conf "printouts.enabled"
  header <- Configurator.lookupDefault defaultHeader conf "printouts.header"
  opts <- Configurator.lookupDefault [] conf "printouts.options"
  return (PrintConf enabled header opts)
  where defaultHeader = "Codex"

getLdapConf :: Config -> IO LdapConf
getLdapConf conf = do
  uri <- Configurator.require conf "ldap.uri"
  base <- Configurator.require conf "ldap.base"
  admins <- Configurator.require conf "ldap.admins"
  return (LdapConf uri base admins)
