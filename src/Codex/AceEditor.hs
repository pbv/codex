--------------------------------------------------------------------------------
-- | Heist splice for Ace Editor
--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
module Codex.AceEditor
       ( inputAceEditorSplices
       , languageMode
       ) where


import qualified Data.Text             as T

import           Data.Maybe(fromMaybe)
import           Data.Map.Syntax
import           Heist
import           Heist.Interpreted
import qualified Text.XmlHtml          as X
import           Text.Printf(printf)

import           Codex.Types
import           Codex.Utils (javascript)

-- make a single XHTML node
element :: Text -> [X.Node] -> [(Text,Text)] -> X.Node
element tag = flip (X.Element tag)


-- | Splice for a textarea/Ace editor form input
-- NB: requires some js "glue" to setup and submit text
inputAceEditor :: (Functor m, Monad m) => Splice m
inputAceEditor = do
    node <- getParamNode
    let children = X.elementChildren node
    let attrs = X.elementAttrs node
    return $ fromMaybe [X.TextNode "inputAceEditor?"] $
      do id <- lookup "id" attrs
         mode <- lookup "mode" attrs
         return [
           element "textarea" children [("name",id)],
           element "div" children [("id", id), ("style", "display:none")],
           javascript $ T.pack $
           printf "startAceEditor('%s','%s');\n"  (T.unpack id) (T.unpack mode)
           ]


inputAceEditorSplices :: (Functor m, Monad m) => Splices (Splice m)
inputAceEditorSplices = "inputAceEditor" ## inputAceEditor


languageMode :: Language -> Text
languageMode (Language l) = case l of
  "c"   -> "c_cpp"
  "cpp" -> "c_cpp"
  l -> l
