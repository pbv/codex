{-# LANGUAGE OverloadedStrings #-}
{- Typeclasses for conversion to/from XHTML
   Pedro Vasconcelos, 2013
-}
module XHTML where

import qualified Data.ByteString as B

import           Data.Char
import           Data.Text (Text)
import qualified Data.Text             as T

import           Text.XmlHtml 

import           Text.Parsec

class ToXHTML a where
  toDocument :: a -> Document
  
class FromXHTML a where
  fromDocument :: Document -> Maybe a
  
                 
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
tagged tag reader = do 
  n <- satisfyNode (\n -> tagName n==Just tag) <?> "<"++ T.unpack tag ++ ">"
  whitespace 
  case parse reader "n/a" (childNodes n) of
    Left msg -> fail (show msg)
    Right result -> return result


-- | consume all text nodes, ignoring others
allText :: XMLReader Text
allText = do ns <- many (satisfyNode isTextNode)
             return (T.concat $ map nodeText ns)


-- | consume all remaining nodes
nodes :: XMLReader [Node]
nodes = many anyToken


-- | parser that consumes whitespace nodes
-- i.e. empty text and comments
whitespace :: XMLReader ()
whitespace = many (satisfyNode white) >> return ()
  where white (TextNode t) = T.all isSpace t
        white (Comment _ ) = True
        white _            = False
        
        
satisfyNode :: (Node -> Bool) -> XMLReader Node        
satisfyNode f = tokenPrim showTok nextPos testTok
  where showTok = show 
        testTok x = if f x then Just x else Nothing
        nextPos pos _ _ = pos  -- don't care about source position
        

-- | empty document        
empty :: XMLReader ()        
empty = eof

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
