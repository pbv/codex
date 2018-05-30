--------------------------------------------------------------------------------
-- | Heist splice for Ace Editor
--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

module Codex.AceEditor
       ( textEditorSplice
       , languageSplices
       -- , languageExtension
       , languageExtensions
       ) where


import qualified Data.Text             as T
import           Data.Maybe(fromMaybe)
import           Data.Map.Syntax
import           Heist
import           Heist.Interpreted
import qualified Text.XmlHtml          as X
import           Text.Printf(printf)
import qualified Data.HashMap.Strict as HM
import           Data.HashMap.Strict (HashMap)

import           Codex.Types
import           Codex.Utils (javascript)

-- make a single XHTML node
element :: Text -> [X.Node] -> [(Text,Text)] -> X.Node
element tag = flip (X.Element tag)


-- | Splice for an Ace text editor form input
-- NB: requires some js "glue" to setup and submit text
textEditor :: (Monad m) => Splice m
textEditor = do
    node <- getParamNode
    let children = X.elementChildren node
    let attrs = X.elementAttrs node
    -- let id = fromMaybe "editor" $ lookup "id" attrs
    return [ element "div" children attrs ]
    {-
    return [
      element "textarea" [] [("name",id), ("style", "display:none")],
      element "div" children [("id", id)]
       javascript $
        T.unlines [
          T.pack $
           printf "var editor = ace.edit(\"%s\");" (T.unpack id)
          , "editor.setFontSize(16);"
          ]
]
-}
  


-- | splice pulldown selector for the submission language
languageSelector :: Monad m => [Language] -> Splice m
languageSelector langs = do
  node <- getParamNode
  let attrs = X.elementAttrs node
  let selected = lookup "selected" attrs
  return [ element "select" (map (option selected . fromLanguage) langs) attrs ]
  where option selected l
          = element "option"  [ X.TextNode l ] ([ ("value", l) ] ++
                              [ ("selected","true") | Just l == selected])
            


languageConstants :: Monad m => [Language] -> Splice m
languageConstants langs =
  return [ javascript $ T.unlines
           [ T.pack  ("var languageModes = " 
                      ++  show (map languageMode langs) ++ ";")
           , T.pack ("var languageExtensions = "
                     ++ show (map languageExtension langs) ++ ";")
           ]
         ]


{-
  let attrs = X.elementAttrs node ++
        [ ("onchange", "editor.session.setMode(languageModes[this.selectedIndex])")
        ]

, javascript $  T.unlines [
             
             , T.pack $ "var languageExtensions = " ++
               show (map languageExtension langs) ++ ";"
             ]
-}
  

textEditorSplice :: (Monad m) => Splices (Splice m)
textEditorSplice = do
  "input-text-editor" ## textEditor

languageSplices :: Monad m =>
  [Language] -> Maybe Language -> Splices (Splice m)
languageSplices langs optSelected = do
  "input-language-selector" ##  languageSelector langs
  "js-language-constants" ## languageConstants langs
  "js-default-language" ## return [
    javascript $ T.pack $
    printf "editor.session.setMode(%s);"
    (maybe "languageModes[0]" (show.languageMode) optSelected)
    ]




-- extensions for HTML form fields 
languageExtensions :: [Language] ->  Text
languageExtensions langs
  = T.intercalate "," $
    map (\lang -> HM.lookupDefault "" lang fileExtension) langs

languageExtension :: Language -> Text
languageExtension l
  = HM.lookupDefault (T.cons '.' $ fromLanguage l) l fileExtension

fileExtension :: HashMap Language Text
fileExtension
  = HM.fromList
    [
      ("c", ".c")
    , ("bash", ".sh")
    , ("cpp", ".cpp")
    , ("csharp", ".cs")
    , ("fsharp", ".fs")
    , ("haskell", ".hs")
    , ("html", ".html")
    , ("java", ".java")
    , ("javascript", ".js")
    , ("json", ".json")
    , ("latex", ".tex")
    , ("ocaml", ".ml")
    , ("prolog", ".pl")
    , ("python", ".py")
    , ("ruby", ".rb")
    , ("pascal", ".pas")
    , ("rust", ".rs")
    , ("scala", ".scala")
    , ("sql", ".sql")
    , ("tcl", ".tcl")
    , ("tex", ".tex")
    , ("xml", ".xml")
    , ("markdown", ".md")
    ]


languageMode :: Language -> Text
languageMode l 
  = HM.lookupDefault (T.append "ace/mode/" (fromLanguage l)) l editorMode

editorMode :: HashMap Language Text
editorMode
  = HM.fromList
    [ ("c", "ace/mode/c_cpp")
    , ("cpp", "ace/mode/c_cpp")
    , ("java", "ace/mode/java")
    , ("haskell", "ace/mode/haskell")
    , ("python", "ace/mode/python")
    , ("ruby", "ace/mode/ruby")
    , ("pascal", "ace/mode/pascal")
    , ("ocaml", "ace/mode/ocaml")
    , ("scala", "ace/mode/scala")
    , ("rust", "ace/mode/rust")
    , ("prolog", "ace/mode/prolog")
    , ("bash", "ace/mode/bash")
    , ("tcl", "ace/mode/tcl")
    , ("sql", "ace/mode/sql")
    , ("tex", "ace/mode/tex")
    , ("latex", "ace/mode/latex")
    , ("html", "ace/mode/html")
    , ("json", "ace/mode/json")
    , ("markdown", "ace/mode/markdown")
    ]
