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

import qualified Data.Text as T
import           Data.Maybe(fromMaybe)
import           Text.Read(readMaybe)
import           Data.Hashable (hash)

import           Control.Exception  (IOException)
import           Control.Exception.Lifted  (try)
import           Control.Monad (guard)
import           Control.Monad.IO.Class (liftIO)

import           Text.Pandoc hiding (Code,
                                      getCurrentTime,
                                      getCurrentTimeZone)
import qualified Text.Pandoc as Pandoc 
import           Text.Pandoc.Walk as Pandoc
import           System.FilePath

import           Snap.Snaplet.Heist


--
-- | view a Markdown page, fill-in exercise links
--
pageView :: UserLogin -> FilePath -> Page -> Codex ()
pageView uid rqpath page = do
  -- check that this is a plain markdown page, not an exercise
  guard (pageTester page == Nothing)  
  let rqdir = takeDirectory rqpath
  (fillExerciseLinks uid rqdir $
    fillUserLinks uid $
    runShuffleing uid page) >>= renderMarkdown

renderMarkdown :: Page -> Codex ()
renderMarkdown page = renderWithSplices "_page" (pageSplices page)



-- | random shuffling for lists inside marked DIV blocks
--
runShuffleing :: UserLogin -> Page -> Page
runShuffleing uid page
  = Rand.run (seed+salt) (walkM shuffleLists page)
  where seed = fromMaybe (hash uid) $ lookupFromMeta "shuffle-seed" meta
        salt = fromMaybe 0 $ lookupFromMeta "shuffle-salt" meta
        meta = pageMeta page

-- | worker functions
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


-- | fill titles and submission counts on exercise links
fillExerciseLinks :: UserLogin -> FilePath -> Page -> Codex Page
fillExerciseLinks uid rqdir page = do
    root <- getDocumentRoot
    walkM (exerciseLinks uid root rqdir) page

-- | fetch title and submissions count for exercise links
exerciseLinks :: UserLogin -> FilePath -> FilePath -> Inline -> Codex Inline
exerciseLinks uid root rqdir (Link attr@(_, classes,_) _ target@(url,_))
  | "ex" `elem` classes = do
      let path = normalise (rqdir </> url)
      title <- liftIO $ readPageTitle (root </> path)
      count <- countSubmissions uid path
      return (formatLink attr title target count)
exerciseLinks _uid _root _rqdir elm
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


fillUserLinks :: UserLogin -> Page -> Page
fillUserLinks uid = walk (userLinks uid)
  
-- jpp hack: replace %u in exercise URLs with the user login
userLinks ::  UserLogin -> Inline -> Inline
userLinks uid (Link attr@(_, classes,_) inlines _target@(url,short))
  | "user" `elem` classes = 
      let url' = replaceUserField (T.unpack $ fromLogin uid) url
          target' = (url', short)
      in (Link attr inlines target')
userLinks _ elm = elm

  
replaceUserField :: String -> String -> String
replaceUserField uid ('%':'u':rest) = uid ++ rest
replaceUserField uid (first:rest) = first : replaceUserField uid rest
replaceUserField _   [] = []



readPageTitle :: FilePath -> IO [Inline]
readPageTitle path = do
  result <- try (readMarkdownFile path)
  return $ case result of
    Left (_ :: IOException) -> brokenLink 
    Right page -> fromMaybe brokenLink (pageTitle page)
  where
    brokenLink = [Pandoc.Code nullAttr path]


markdownHandlers :: Handlers Codex
markdownHandlers = mempty { handleView = pageView }
