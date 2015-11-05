
module Markdown where

import           Text.Pandoc
import           Text.Pandoc.Builder
import           Text.XmlHtml 
import           Text.Blaze.Renderer.XmlHtml

-- acessors for markdown attributes
classes :: Attr -> [String]
classes (_, cs, _) = cs

keyValues :: Attr -> [(String, String)]
keyValues (_, _, kvs) = kvs

ident :: Attr -> String
ident (id, _, _ ) = id

headerAttr :: Block -> Attr
headerAttr (Header _ attr _) = attr
headerAttr _                 = error "headerAttr: not a header block"

headerLevel :: Block -> Int
headerLevel (Header n _ _) = n
headerLevel _              = error "headerLever: not a header block"

headerInlines :: Block -> Inlines
headerInlines (Header _ _ ls) = fromList ls
headerInlines _              = error "headerInlines: not a header block"

{-
-- file extensions and associated Pandoc readers
readersList :: [(String, String -> Pandoc)]
readersList
  = [(ext, readMarkdown myReaderOptions) | ext<-[".md",".mdown",".markdown"]] ++
    [(ext, readHtml myReaderOptions)     | ext<-[".html", ".htm"]] ++
    [(".tex", readLaTeX myReaderOptions)]
-}


{-
-- | class for rendering HTML markup
class HTML a where
  renderNodes :: a -> [Node]

-- | render a Pandoc document into a list of HTML nodes
instance HTML Pandoc where
  renderNodes  = renderHtmlNodes . writeHtml myWriterOptions 

instance HTML Block where
  renderNodes = renderNodes . doc . singleton 

instance HTML Inline where
  renderNodes = renderNodes . doc . from
-}


pandocToHtml :: Pandoc -> [Node]
pandocToHtml = renderHtmlNodes . writeHtml myWriterOptions

blocksToHtml :: Blocks -> [Node]
blocksToHtml = pandocToHtml . doc

inlinesToHtml :: Inlines -> [Node]
inlinesToHtml = blocksToHtml . plain


--- pandoc reader and writter options
myReaderOptions :: ReaderOptions
myReaderOptions = def { readerExtensions = pandocExtensions
                      , readerSmart = True 
                      }

myWriterOptions :: WriterOptions
myWriterOptions = def { writerExtensions = pandocExtensions
                      , writerHTMLMathMethod = MathJax "/mathjax",
                        writerHighlight = True
                      }
