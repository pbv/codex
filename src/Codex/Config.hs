{-# LANGUAGE OverloadedStrings #-}
module Codex.Config where

import qualified Data.HashMap.Strict as HashMap
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

{-
-- | constant paths
publicPath :: FilePath
publicPath = "public"

staticPath :: FilePath
staticPath = "static"
-}

-- | custom mime type mapping
mimeTypes :: MimeMap
mimeTypes
  = HashMap.union defaultMimeTypes $
    HashMap.fromList [(".tst", "text/plain"),
                      (".py",  "text/plain"),
                      (".mdown", "text/markdown"),
                      (".md",  "text/markdown"),
                      (".db", "application/x-sqlite3")]
