{-# LANGUAGE OverloadedStrings #-}
module Codex.Config where

import qualified Data.HashMap.Strict as HM
import           Snap.Util.FileServe(MimeMap, defaultMimeTypes)

-- | custom mime type mapping
mimeTypes :: MimeMap
mimeTypes
  = HM.union defaultMimeTypes $
    HM.fromList [(".mdown", "text/markdown"),
                 (".md",  "text/markdown")
                ]
