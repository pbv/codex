{-# LANGUAGE RecordWildCards #-}
{- 
-- Manage threads for evaluation tasks
-}
module Codex.Tasks
  ( TaskSemph,
    TaskQueue,
    newTaskSemph,
    forkTask,
    newTaskQueue,
    addTask,
    cancelTasks,
  ) where

import           Control.Monad.IO.Class
import           Control.Concurrent
import           Control.Exception (bracket_)

-- | a task semaphore of for limiting concurrency
newtype TaskSemph = TaskSemph { taskSemph :: QSem }

-- | a task queue for canceling evaluations
newtype TaskQueue = TaskQueue { taskQueue :: MVar [ThreadId] }


-- | create a new semaphore with a given number of simulateanous tasks
newTaskSemph :: MonadIO m => Int -> m TaskSemph
newTaskSemph n = TaskSemph <$> (liftIO $ newQSem n)


-- | fork an IO action controlled by a task semaphore;
-- only the specified maximum number of tasks can run at the same time
forkTask :: MonadIO m => TaskSemph -> IO () -> m ThreadId
forkTask TaskSemph{..} action
  =  liftIO $ forkIO $ bracket_ (waitQSem taskSemph) (signalQSem taskSemph) action
    

-- create a new empty queue
newTaskQueue :: MonadIO m => m TaskQueue
newTaskQueue  = liftIO $ TaskQueue <$> newMVar []

-- | add an IO action to a queue
addTask :: MonadIO m => TaskQueue -> IO ThreadId -> m ()
addTask TaskQueue{..} action
  = liftIO $ modifyMVar_ taskQueue $ \tids -> do tid<-action; return (tid:tids)

-- | cancel all tasks in a queue
cancelTasks :: MonadIO m => TaskQueue -> m ()
cancelTasks TaskQueue{..}
  = liftIO $ modifyMVar_ taskQueue (\ids -> mapM_ killThread ids >> return [])





