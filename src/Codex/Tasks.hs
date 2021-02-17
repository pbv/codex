--
-- Manage asynchronous evaluation tasks
--
module Codex.Tasks (
   TaskGroup, Queue,
   createTaskGroup, forkTask,
   createQueue, addQueue, cancelAll
   ) where

import           Control.Monad.IO.Class
import           Control.Concurrent
import           Control.Exception (bracket_)


-- | a task group
-- a quantity semaphore to throttle the number of concurrent threads
newtype TaskGroup = TaskGroup QSem

-- Create a task group with n concurrent slots
createTaskGroup :: MonadIO m => Int -> m TaskGroup
createTaskGroup n = liftIO (TaskGroup <$> newQSem n)

-- Fork a new IO action under a task group
forkTask :: MonadIO m => TaskGroup -> IO () -> m ThreadId
forkTask (TaskGroup qsem) action 
  = liftIO $ forkIO (bracket_ (waitQSem qsem) (signalQSem qsem) action)

-- | a queue for pending evaluations
newtype Queue = Queue (MVar [ThreadId])

createQueue :: MonadIO m => m Queue
createQueue 
  = liftIO (Queue <$> newMVar [])

addQueue :: MonadIO m => ThreadId -> Queue -> m ()
addQueue threadId (Queue v)
  = liftIO $ modifyMVar_ v (\list -> return (threadId:list))

cancelAll :: MonadIO m => Queue -> m ()
cancelAll (Queue v)
  = liftIO $ modifyMVar_ v (\list -> mapM_ killThread list >> return [])

