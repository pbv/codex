{-# LANGUAGE OverloadedStrings #-}
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
import           Data.Maybe(fromMaybe, isNothing)
import           Text.Read(readMaybe)
import           Data.Hashable (hash)

import           Control.Monad (guard)

import           Text.Pandoc hiding (Code,
                                      getCurrentTime,
                                      getCurrentTimeZone)

import           Text.Pandoc.Walk as Pandoc
import           System.FilePath

import           Snap.Snaplet.Heist


--
-- | view a Markdown page, fill-in exercise links
--
pageView :: UserLogin -> FilePath -> Page -> Codex ()
pageView uid rqpath page = do
  -- check that this is a plain markdown page, not an exercise
  guard (isNothing (pageTester page))
  let rqdir = takeDirectory rqpath
  fillExerciseLinks uid rqdir 
     (fillUserLinks uid $
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
      let limit = (readMaybe . T.unpack) =<< lookup "choose" attrs
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
      let path = normalise (rqdir </> T.unpack url)
      title <- readMarkdownTitle (root </> path)
      submitted <- countSubmissions uid path
      accepted <- countAccepted uid path
      return (formatExerciseLink attr title target submitted accepted)
exerciseLinks _uid _root _rqdir elm
  = return elm

-- | format an exercise link with a tooltip
formatExerciseLink attr title target submitted accepted
  = Span ("", ["tooltip"], [])
    [Link attr title target,
      if accepted>0
      then Span ("", ["Accepted"], []) [ Space, Str "\x2705" ]
      else Space
    , Span ("", ["info"], [])
       [ Str (T.pack $ show submitted), Space, Str "submissions" ]
    ]


fillUserLinks :: UserLogin -> Page -> Page
fillUserLinks uid = walk (userLinks uid)

-- jpp hack: replace %u in exercise URLs with the user login
userLinks ::  UserLogin -> Inline -> Inline
userLinks uid (Link attr@(_, classes,_) inlines _target@(url,short))
  | "user" `elem` classes =
      let url' = T.pack $ replaceUserField (T.unpack $ fromLogin uid) (T.unpack url)
          target' = (url', short)
      in Link attr inlines target'
userLinks _ elm = elm


replaceUserField :: String -> String -> String
replaceUserField uid ('%':'u':rest) = uid ++ rest
replaceUserField uid (first:rest) = first : replaceUserField uid rest
replaceUserField _   [] = []



markdownHandlers :: Handlers Codex
markdownHandlers = mempty { handleView = pageView }
