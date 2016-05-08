{-# LANGUAGE OverloadedStrings #-}
module Config where

import           Types
import           SafeExec

import           Data.Monoid
import qualified Data.Configurator as Configurator
import           Data.Configurator.Types



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
