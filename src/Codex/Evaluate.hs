{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Codex.Evaluate(
  -- * evaluate a single submission
  evaluate,
  evaluateWith 
  ) where

import qualified Data.Text                                   as T
import           Data.Monoid
import           Data.Maybe
import           Data.Time.LocalTime
import           Control.Monad.State
import           Control.Concurrent(ThreadId, forkIO)
import           Control.Exception  (SomeException)
import           Control.Exception.Lifted  (catch)
import           System.FilePath

import           Snap.Snaplet
import qualified Snap.Snaplet.SqliteSimple                   as S

import           Codex.Application
import           Codex.Types
import           Codex.Utils
import           Codex.Page
import           Codex.Submission
import           Codex.Tester
import           Codex.Testers
import           Codex.Interval



-- | default evaluator
evaluate :: Submission -> Codex ThreadId
evaluate sub = do
  -- tester <- gets defaultTester
  evaluateWith allTesters sub


-- evaluate a submission  with a specific tester
-- (1st time or re-evaluation);
-- runs code tester in separate thread
-- uses a semaphore for "throttling" evaluations 
evaluateWith :: Tester Result -> Submission -> Codex ThreadId
evaluateWith tester sub = do
  sqlite <- S.getSqliteState
  evs <- getEvents
  root <- getDocumentRoot
  conf <- getSnapletUserConfig
  sem <- gets evalSem
  liftIO $ forkIO $ withQSem sem $ do
    tz <- getCurrentTimeZone
    let filepath = root </> submitPath sub
    page <- readMarkdownFile filepath
            `catch` (\(_ :: SomeException) -> return emptyPage)
    let sid = submitId sub        -- ^ submission number
    let optT = rankTime (submitTime sub) <$> evalI tz evs (submitInterval page)
    case optT of
      Nothing ->
        updateSubmission sqlite sid (wrongInterval page) Valid
      Just t -> do
        putStrLn $ "start evaluation of submission " ++ show sid
        let code = submitCode sub     -- ^ program code
        mayResult <- runTester conf filepath page code tester
                     `catch`
                     (\(e::SomeException) ->
                         return (Just $ miscError $ T.pack $ show e))
        let result = fromMaybe (missingTester code) mayResult
        updateSubmission sqlite sid result t
        putStrLn $ "end evaluation of submission " ++ show sid


wrongInterval :: Page -> Result
wrongInterval page =
  let valid = fromMaybe "" $ lookupFromMeta "valid" (pageMeta page)
  in miscError $ "Invalid submission interval \"" <> valid <> "\""
  

missingTester :: Code -> Result
missingTester code = miscError $
  "No tester defined for language \"" <> fromLanguage (codeLang code) <> "\""

