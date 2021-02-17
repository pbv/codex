{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Codex.Evaluate(
  evaluateNew,
  evaluate,
  evaluateMany,
  cancelPending,
  ) where

import qualified Data.Text as T
import           Data.Maybe
import           Data.Time (getCurrentTime)
import           Control.Monad.Writer
import           Control.Monad.State
import           Control.Monad.Reader
import           Control.Concurrent (ThreadId)
import           Control.Exception  (SomeException, catch)
import           System.FilePath

import           Snap.Snaplet
import qualified Snap.Snaplet.SqliteSimple as S


import           Data.Configurator.Types(Config)
import qualified Data.Configurator as Conf

import           Codex.Types
import           Codex.Application
import           Codex.Tasks
import           Codex.Utils
import           Codex.Page
import           Codex.Submission 
import           Codex.Tester
import           Codex.Policy


-- | insert a new submission into the database
-- also schedule evaluation in separate thread
evaluateNew :: UserLogin -> FilePath -> Code -> Codex SubmitId
evaluateNew uid rqpath code = do
  now <- liftIO getCurrentTime
  sub <- newSubmission uid rqpath now code evaluating pending
  evaluate sub
  return (submitId sub)

pending :: Validity
pending = invalid "Waiting evaluation"

-- | evaluate a submission
-- use the global task group for limiting concurrency
evaluate :: Submission -> Codex ThreadId
evaluate submission = do
  taskGroup <- gets _taskGroup
  evaluateWith taskGroup submission

-- | re-evaluate a list of submissions;
-- create a new task group for limiting concurrency;
-- add tasks to the global queue to allow canceling
evaluateMany :: [Submission] -> Codex ()
evaluateMany submissions = do
  conf <- getSnapletUserConfig
  taskGroup <- liftIO $
               createTaskGroup =<< Conf.require conf "system.max_concurrent"
  pending <- gets _queue
  forM_ submissions
    (\sub -> do threadId <- evaluateWith taskGroup sub
                addQueue threadId pending) 


-- | cancel all pending evaluations
cancelPending :: Codex ()
cancelPending
  = cancelAll =<< gets _queue


-- | evaluate a single submission with a quantity semaphore
--
evaluateWith :: TaskGroup -> Submission -> Codex ThreadId
evaluateWith taskGroup submission@Submission{..} = do
  conn <- S.getSqliteState
  conf <- getSnapletUserConfig
  timeEnv <- getTimeEnv
  tester <- gets _tester
  root <- getDocumentRoot
  let filepath = root </> submitPath
  forkTask taskGroup $ do
    -- run the tester code asynchronously
    runSqlite conn $ updateResult submitId evaluating pending
    page <- readMarkdownFile filepath
    let policy = getPolicy page
    check <- runSqlite conn $ checkPolicy timeEnv policy submission 
    result <- testWrapper tester conf filepath page submission
    runSqlite conn $ updateResult submitId result check


-- | wrapper to run a tester and catch any exceptions
testWrapper :: Tester Result
            -> Config
            -> FilePath
            -> Page
            -> Submission
            -> IO Result
testWrapper tester conf filepath page submiss
  = (fromMaybe invalid <$> runTester tester conf filepath page submiss)
    `catch`
    (\(e::SomeException) -> return (miscError $ T.pack $ show e))
  where
    invalid :: Result
    invalid = miscError "no acceptable tester configured"


runSqlite :: S.Sqlite -> ReaderT S.Sqlite m a -> m a
runSqlite conn  = flip runReaderT conn
