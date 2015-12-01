--------------------------------------------------------------------------------
-- | Heist splice for Ace Editor
--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
module AceEditor
       (inputAceEditorSplices
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

-- make a single XHTML node
element :: Text -> [X.Node] -> [(Text,Text)] -> X.Node
element tag nodes = flip (X.Element tag) nodes

--------------------------------------------------------------------------------
{-
getAttributes :: Monad m =>
                   HeistT m m (Text, [(Text, Text)])  -- ^ (id attr, other attrs)
getAttributes = do
    node <- getParamNode
    return $ case node of
        X.Element _ as _ ->
            let id_ = fromMaybe (error $ show node ++ ": missing id attribute") $
                        lookup "id" as 
            in (id_, filter ((/= "id") . fst) as)
        _                -> error "invalid node"
-}


{-
--------------------------------------------------------------------------------
-- | Does not override existing attributes
addAttrs :: [(Text, Text)]  -- ^ Original attributes
         -> [(Text, Text)]  -- ^ Attributes to add
         -> [(Text, Text)]  -- ^ Resulting attributes
addAttrs = unionBy (on (==) fst)
-}

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

{-
-- | Splice for a textarea/Ace editor form input
-- NB: requires some js "glue" to setup and submit text
inputAceEditor :: (Functor m, Monad m) => Splice m
inputAceEditor = do
    (txt_id, attrs) <- getAttributes
    children <- X.elementChildren <$> getParamNode 
    let div_id = T.append txt_id ".div"
        textarea = element "textarea" children $
                   addAttrs attrs [("id", txt_id), ("name", txt_id)]
        editor = element "div" children $
                 addAttrs attrs [("id", div_id), ("style", "display:none")]
    return [textarea, editor]
-}

inputAceEditorSplices :: (Functor m, Monad m) => Splices (Splice m)
inputAceEditorSplices = "inputAceEditor" ## inputAceEditor
