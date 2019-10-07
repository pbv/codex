{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
--
-- Markdown view handler
--
module Codex.Handlers.Markdown
  ( markdownHandlers
  ) where


import           Codex.Types
import           Codex.Application
import           Codex.Handlers
import           Codex.Page
import           Codex.Random(Rand)
import qualified Codex.Random as Rand
import           Codex.Utils
import           Codex.Submission

import           Data.Maybe(fromMaybe)
import           Text.Read(readMaybe)
import           Data.Hashable (hash)

import           Control.Exception  (IOException)
import           Control.Exception.Lifted  (catch)
import           Control.Monad.IO.Class (liftIO)

import           Text.Pandoc hiding (Code,
                                     getCurrentTime,
                                     getCurrentTimeZone)
import qualified Text.Pandoc as Pandoc 
import           Text.Pandoc.Walk as Pandoc

import           Snap.Snaplet.Heist

import           System.FilePath


--
-- | view a Markdown page, fill-in exercise links
--
pageView :: UserLogin -> FilePath -> Page -> Codex ()
pageView uid rqpath page = do
  let rqdir = takeDirectory rqpath
  renderMarkdown =<< fillExerciseLinks uid rqdir page

renderMarkdown :: Page -> Codex ()
renderMarkdown page = renderWithSplices "_page" (pageSplices page)


-- | fill titles and submission counts on exercise links
fillExerciseLinks :: UserLogin -> FilePath -> Page -> Codex Page
fillExerciseLinks uid rqdir page = do
    root <- getDocumentRoot
    let meta = pageMeta page
    let seed = fromMaybe (hash uid) $ lookupFromMeta "random-seed" meta
    walkM (fetchLink uid root rqdir) $
      Rand.run seed (walkM shuffleLists page) 

-- | shuffle exercise lists inside a marked container 
shuffleLists :: Block -> Rand Block
shuffleLists (Div (id, classes, attrs) blocks)
  | "shuffle" `elem` classes = do
      let limit = readMaybe =<< lookup "choose" attrs 
      blocks' <- mapM (shuffleList limit) blocks
      return (Div (id, classes, attrs) blocks')
shuffleLists block
  = return block

shuffleList :: Maybe Int -> Block -> Rand Block
shuffleList limit (OrderedList attrs list) =
  OrderedList attrs <$> shuffle' limit list
shuffleList limit (BulletList list) =
  BulletList <$> shuffle' limit list
shuffleList _ block =
  return block

shuffle' :: Maybe Int -> [a] -> Rand [a]
shuffle' limit xs = maybe id take limit <$> Rand.shuffle xs 

  
-- | fetch title and submissions count for exercise links
fetchLink :: UserLogin -> FilePath -> FilePath -> Inline -> Codex Inline
fetchLink uid root rqdir (Link attr@(_, classes,_) _ target@(url,_))
  | "ex" `elem` classes = do
      let path = normalise (rqdir </> url)
      title <- liftIO $ readPageTitle (root </> path)
      count <- countPageSubmissions uid path
      return (formatLink attr title target count)
fetchLink uid root rqdir elm
  = return elm

-- | format a single exercise link
formatLink attr title target count
  = Span nullAttr
    [Link attr title target,
     LineBreak,
      Span ("", ["info"], [])
      [Str "(",
        Str (show count), Space, Str "submissões",
        Str ")"]
    ]

readPageTitle :: FilePath -> IO [Inline]
readPageTitle path
  = fromMaybe [Pandoc.Code nullAttr path] <$> readTitle
  where
    readTitle = (pageTitle <$> readMarkdownFile path)
                `catch` (\(_ :: IOException) -> return Nothing)


markdownHandlers :: Handlers Codex
markdownHandlers = mempty { handleView = pageView }
