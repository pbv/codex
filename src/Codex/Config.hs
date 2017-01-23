{-# LANGUAGE OverloadedStrings #-}
module Codex.Config where

import qualified Data.HashMap.Strict as HM
import           Snap.Util.FileServe(MimeMap, defaultMimeTypes)

{-
getPrintConf :: Config -> IO PrintConf
getPrintConf conf = do
  enabled <- Configurator.lookupDefault False conf "printouts.enabled"
  header <- Configurator.lookupDefault defaultHeader conf "printouts.header"
  opts <- Configurator.lookupDefault [] conf "printouts.options"
  return (PrintConf enabled header opts)
  where defaultHeader = "Codex"
-}



-- | custom mime type mapping
mimeTypes :: MimeMap
mimeTypes
  = HM.union defaultMimeTypes $
    HM.fromList [(".tst", "text/plain"),
                 (".py",  "text/plain"),
                 (".mdown", "text/markdown"),
                 (".md",  "text/markdown"),
                 (".db", "application/x-sqlite3")]
