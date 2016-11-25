--------------------------------------------------------------------------------
-- | Heist splice for Ace Editor
--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
module AceEditor
       ( inputAceEditorSplices
       , languageMode
       ) where
import           Control.Applicative((<$>), (<*>))
import           Data.Maybe            (fromMaybe)
import           Data.Text             (Text)
import qualified Data.Text             as T
--import           Data.Function         (on)
--import           Data.List             (unionBy)

import           Heist
import           Heist.Interpreted
import qualified Text.XmlHtml          as X

import           Utils (javascript)
import           Text.Printf(printf)

import           Language.Types 

-- make a single XHTML node
element :: Text -> [X.Node] -> [(Text,Text)] -> X.Node
element tag nodes = flip (X.Element tag) nodes


-- | Splice for a textarea/Ace editor form input
-- NB: requires some js "glue" to setup and submit text
inputAceEditor :: (Functor m, Monad m) => Splice m
inputAceEditor = do
    node <- getParamNode
    let children = X.elementChildren node
    let attrs = X.elementAttrs node
    return $ maybe [X.TextNode "inputAceEditor?"] id $ 
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
  "c++" -> "c_cpp"
  "cpp" -> "c_cpp"
  l -> l


