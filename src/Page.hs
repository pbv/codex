{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-
  Data types and methods for exercise pages
-}
module Page where

import           Control.Applicative 
import           Control.Monad

import           Data.Maybe
import           Data.Text(Text)
import qualified Data.Text             as T

import           Data.Time.LocalTime

import           Language.Types
import           Markdown
import           Interval
import           Text.Pandoc hiding (Code)
import           Text.Pandoc.Walk

import           System.FilePath


-- | a document page; either a single exercise or an index
data Page  
  = Page { root :: FilePath        -- file root dir for this page
         , path :: FilePath        -- relative path
         , meta :: Meta
         , description :: [Block]
         , fetched :: ZonedTime     -- time fetched
         } deriving Show




parent :: FilePath -> FilePath
parent path = takeDirectory path </> "index.md"


-- | fetch page title
getTitle :: Page -> Text
getTitle Page{..}
  = fromMaybe (T.pack path) 
    (lookupFromMeta "title" meta <|>  firstHeader description)

  
-- help function;
-- get first header frm a list of blocks
firstHeader :: [Block] -> Maybe Text
firstHeader blocks
  = listToMaybe [query inlineText h | Header _ _ h <- blocks]

-- | fetch page tags
getTags :: Page -> [Text]
getTags Page{..}
  = fromMaybe [] (lookupFromMeta "tags" meta)

{-
-- | check if a page has all tags
isTagged :: [Text] -> Page -> Bool
isTagged tags page = tags `isSublistOf` getTags page
    where isSublistOf xs ys = all (`elem`ys) xs
-}

getLanguage :: Page -> Maybe Language
getLanguage Page{..}
  = lookupFromMeta "language" meta

getCodeText :: Page  -> Maybe Text
getCodeText Page{..}
  = lookupFromMeta "code" meta

getCode :: Page -> Maybe Code
getCode page
  = Code <$> getLanguage page <*> getCodeText page



-- | read a page from a markdown file
readPage :: FilePath -> FilePath -> IO Page
readPage root path = do
  let filepath = root </> path
  Pandoc meta blocks <- readMarkdownFile filepath
  t <- getZonedTime
  return Page { root = root
              , path = path
              , meta = meta
              , description = blocks
              , fetched = t
              }



getLinks :: Page -> Maybe [FilePath]
getLinks Page{..} = do
      let dir = takeDirectory path   -- directory of page path
      paths <- lookupFromMeta "index" meta
      return $ map (normalise.(dir</>)) paths 

validInterval :: Page -> Interval
validInterval = fromMaybe Always . validInterval'

validInterval' :: Page -> Maybe Interval
validInterval' Page{..} = lookupFromMeta "valid" meta >>= readInterval fetched

isExercise :: Page -> Bool
isExercise Page{..} =
  fromMaybe False (lookupFromMeta "exercise" meta)




