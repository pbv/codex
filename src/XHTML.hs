{-# LANGUAGE OverloadedStrings #-}
{- Helper functions for convertion to/from XHTML
   Pedro Vasconcelos, 2013
-}
module XHTML where

import qualified Data.ByteString.Char8 as B

import           Data.Char
import           Data.Text (Text)
import qualified Data.Text             as T

import           Text.XmlHtml 
import           Blaze.ByteString.Builder

import           Text.Parsec
import           Text.Parsec.Pos


-- constructors for HTML and XML documents
htmlDocument :: [Node] -> Document
htmlDocument ns 
  = HtmlDocument { docEncoding = UTF8, docType = Nothing, docContent = ns }
                     
xmlDocument :: [Node] -> Document                
xmlDocument ns 
  =  XmlDocument { docEncoding = UTF8, docType = Nothing, docContent = ns }
                


-- | type for XML readers
-- parser functions from nodes to something
type XMLReader a = Parsec [Node] () a
  
-- | apply a reader inside a tagged element
-- ignores leading whitespace
tagged :: Text -> XMLReader a -> XMLReader a
tagged tag reader = 
  do --pos <- getPosition 
     n <- satisfyNode (\n -> tagName n==Just tag) <?> "<"++ T.unpack tag ++ ">"
     whitespace
     case parse reader (T.unpack tag) (childNodes n) of
       Left msg -> fail (show msg)
       Right result -> return result

    

-- | consume text nodes, returning all text content
allText :: XMLReader Text
allText = do ns <- many (satisfyNode isTextNode)
             return (T.concat $ map nodeText ns)


-- | consume all remaining nodes
nodes :: XMLReader [Node]
nodes = many (satisfyNode (\_ -> True))


-- | parser that consumes `whitespace'
-- i.e. empty text and comments
whitespace :: XMLReader ()
whitespace = many (satisfyNode isBlankNode) >> return ()
        
-- |  check if a node consist only with whitespace
isBlankNode :: Node -> Bool
isBlankNode (TextNode t) = T.all isSpace t
isBlankNode (Comment _)  = True
isBlankNode _            = False

        
satisfyNode :: (Node -> Bool) -> XMLReader Node        
satisfyNode f = tokenPrim showTok nextPos testTok
  where testTok x = if f x then Just x else Nothing
        nextPos pos n _ = updatePosString pos (nodeToString n)

-- | render a node to a string
nodeToString :: Node -> String
nodeToString (TextNode t) = T.unpack t
nodeToString (Comment t)  = T.unpack t
nodeToString n            = B.unpack $ toByteString $ renderHtmlFragment UTF8 [n]


showTok :: Node -> String
showTok (TextNode t)      = "text " ++ T.unpack t
showTok (Comment t)       = "comment " ++ T.unpack t
showTok (Element tag _ _) = "<" ++ T.unpack tag ++ ">"


-- | no more nodes  
empty :: XMLReader ()
-- empty = eof
empty = notFollowedBy (fmap showTok anyNode) <?> "end"
  where
    anyNode = tokenPrim showTok (\pos _ _ -> pos) Just 


-- | Read an HTML document from a file
-- throws an exception if parsing fails
readHTMLFile :: FilePath -> IO Document
readHTMLFile file = do
  bs <- B.readFile file
  case parseHTML file bs of
    Left msg -> abort msg
    Right doc -> return doc
  where abort msg = ioError $ userError msg
                 
-- | apply a reader to a file
-- throws an exception if parsing fails
readFromHTMLFile :: FilePath -> XMLReader a -> IO a
readFromHTMLFile file reader = do
  doc <- readHTMLFile file
  case parse reader file (docContent doc) of
    Left msg -> abort msg
    Right result -> return result
  where abort msg = ioError $ userError $ show msg
