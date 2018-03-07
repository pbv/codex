{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Codex.Evaluate(
  -- * evaluate a single submission
  evaluate,
  evaluateWith 
  ) where

import qualified Data.Text as T
import           Data.Monoid
import           Data.Maybe
import           Data.Time.LocalTime
import           Control.Monad.State
import           Control.Concurrent(ThreadId, forkIO)
import           Control.Exception  (SomeException)
import           Control.Exception.Lifted  (catch)
import           System.FilePath

import           Snap.Snaplet
import qualified Snap.Snaplet.SqliteSimple as S

import qualified Data.Configurator as Conf

import           Codex.Application
import           Codex.Types
import           Codex.Utils
import           Codex.Page
import           Codex.Submission
import           Codex.Tester
import           Codex.Interval



-- | default evaluator
evaluate :: Submission -> Codex ThreadId
evaluate sub = do
  tester <- gets _tester
  evaluateWith tester sub


-- | evaluate a submission  with a specific tester
-- (1st time or re-evaluation);
-- runs code tester in separate thread
-- uses a semaphore for limitting concurrency 
evaluateWith :: Tester -> Submission -> Codex ThreadId
evaluateWith tester sub = do
  sqlite <- S.getSqliteState
  evs <- getEvents
  root <- getDocumentRoot
  conf <- getSnapletUserConfig
  qs <- gets _evqs
  let filepath = root </> submitPath sub    -- ^ file path to exercise 
  let sid = submitId sub                    -- ^ submission number
  let code = submitCode sub                 -- ^ program code
  liftIO $ forkIO $ withQSem qs $ do
    tz <- getCurrentTimeZone
    page <- readMarkdownFile filepath
    let opt = rankTime (submitTime sub) <$> evalI tz evs (submitInterval page)
    case opt of
      Nothing ->
        updateSubmission sqlite sid (wrongInterval page) Valid
      Just timing -> do
        result <- runLanguageTester conf filepath page code tester
                  `catch`
                  (\(e::SomeException) -> return (miscError $ T.pack $ show e))
        updateSubmission sqlite sid result timing

-- | set default limits and run a tester
runLanguageTester cfg filepath page code tester = do
  limits <- configLimits (Conf.subconfig "limits" cfg)
  fromMaybe (invalidTester code) <$>
    runTest cfg limits (tester filepath page code)

wrongInterval :: Page -> Result
wrongInterval page =
  let valid = fromMaybe "" $ lookupFromMeta "valid" (pageMeta page)
  in miscError $ "Invalid submission interval \"" <> valid <> "\""
 

invalidTester :: Code -> Result
invalidTester (Code lang _)
  = miscError $ "No tester for language: " <> fromLanguage lang 


