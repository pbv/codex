{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-
  Data types and methods for exercise pages
-}
module Page where

import           Control.Monad
import           Control.Applicative 

import           Data.Maybe
import           Data.Monoid
import           Data.String
import           Data.Text(Text)
import qualified Data.Text             as T

import           Data.Time.LocalTime

import           Language.Types
import           Markdown
import           Types
import           Interval
import           Text.Pandoc hiding (Code)
import           Text.Pandoc.Walk


import           System.FilePath
import           System.Directory


-- | a page: either a single problem or a worksheet
data Page
  = Page { root :: FilePath        -- file root dir for this page
         , path :: FilePath        -- relative path
         , meta :: Meta
         , description :: [Block]
         , links :: [FilePath]     -- linked documents (maybe empty)
         , interval :: Interval    -- submission interval (for exercises)
         } deriving Show



parent :: FilePath -> FilePath
parent path = takeDirectory path </> "index.md"

-- | an exercise page has empty list of links 
isExercise :: Page -> Bool
isExercise Page{..} = null links
                         

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

getLanguage :: Page -> Language
getLanguage Page{..}
  = fromMaybe (Language "text") (lookupFromMeta "language" meta)

getCodeText :: Page -> Maybe Text
getCodeText Page{..}
  = lookupFromMeta "code" meta

getCode :: Page -> Maybe Code
getCode page
  = Code (getLanguage page) <$> getCodeText page



-- | read a page from a markdown file
readPage :: FilePath -> FilePath -> IO Page
readPage root path = do
  let filepath = root </> path
  Pandoc meta blocks <- readMarkdownFile filepath
  -- valid submission interval
  t <- getZonedTime
  let valid = fromMaybe Always (lookupFromMeta "valid" meta >>= readInterval t)
  -- interpret linked documents relative to current page directory
  let dir = takeDirectory path
  let paths =  map (normalise.(dir</>)) $
               fromMaybe [] $ lookupFromMeta "index" meta
  return Page { root = root
              , path = path
              , meta = meta
              , description = blocks
              , links = paths
              , interval = valid
              }


