{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Codex.Evaluate(
  -- * evaluate a single submission
  evaluate,
  reevaluate,
  cancelPending
  ) where

import qualified Data.Text as T
import           Data.Maybe
import           Data.Time.LocalTime
import           Control.Monad.State
import           Control.Concurrent(ThreadId) 
import           Control.Exception  (SomeException, catch)
import           System.FilePath

import           Snap.Snaplet
import qualified Snap.Snaplet.SqliteSimple as S

import           Data.Configurator.Types(Config)

import           Codex.Application
import           Codex.Tasks
import           Codex.Utils
import           Codex.Page
import           Codex.Submission
import           Codex.Tester
import           Codex.Interval



-- | evaluate a single submissions
evaluate :: Submission -> Codex ThreadId
evaluate sub = do
  tester <- gets _tester
  semph <- gets _semph
  action <- evaluatorWith tester sub
  liftIO $ forkSingle semph action

-- | re-evaluate a list of submissions
reevaluate :: [Submission] -> Codex ()
reevaluate subs = do
  tester <- gets _tester
  semph <- gets _semph
  tasks <- gets _tasks
  actions <- mapM (evaluatorWith tester) subs
  liftIO $ forkMany semph tasks actions

-- | cancel pending evaluations
cancelPending :: Codex ()
cancelPending = reevaluate []


-- | evaluate a submission with a specific tester
-- (1st time or re-evaluation);
-- runs code tester in separate thread
-- uses a semaphore for limiting concurrency 
evaluatorWith :: Tester Result -> Submission -> Codex (IO ())
evaluatorWith tester sub = do
  sqlite <- S.getSqliteState
  evs <- getEvents
  root <- getDocumentRoot
  conf <- getSnapletUserConfig
  let filepath = root </> submitPath sub  -- ^ file path to exercise 
  let sid = submitId sub                  -- ^ submission number
  let code = submitCode sub               -- ^ program code
  return $ do                             -- ^ return evaluation IO action
    tz <- getCurrentTimeZone
    meta <- pageMeta <$> readMarkdownFile filepath
    let opt = rankTime (submitTime sub) <$>
              evalI tz evs (metaInterval meta)
    case opt of
      Nothing ->
        updateSubmission sqlite sid wrongInterval Valid
      Just timing -> do
        result <- testerWrapper conf filepath code meta tester
                  `catch`
                  (\(e::SomeException) ->
                      return (miscError $ T.pack $ show e))
        updateSubmission sqlite sid result timing

-- | set default limits and run a tester
testerWrapper cfg path code meta action
  = fromMaybe invalidTester <$> runTester cfg meta path code action

wrongInterval :: Result
wrongInterval = miscError "invalid submission interval"
 
invalidTester :: Result
invalidTester = miscError "no acceptable tester configured"


