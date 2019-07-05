{-# LANGUAGE RecordWildCards #-}
{- 
-- Manage threads for evaluation tasks
-}
module Codex.Tasks
  ( Queue,
    newQueue,
    addQueue,
    cancelQueue,
    forkQSem,
  ) where

import           Control.Monad.IO.Class
import           Control.Concurrent
import           Control.Exception (bracket_)


-- | a queue for canceling IO threads (e.g. re-evaluations)
-- mutable list of thread ids
newtype Queue = Queue { threadList :: MVar [ThreadId] }

newQueue :: MonadIO m => m Queue
newQueue  = liftIO $ Queue <$> newMVar []

--
-- | add an IO action to a queue
--
addQueue :: MonadIO m => Queue -> IO ThreadId -> m ()
addQueue Queue{..} action
  = liftIO $ modifyMVar_ threadList $ \tids -> do tid<-action; return (tid:tids)


-- | cancel all pending tasks
cancelQueue :: MonadIO m => Queue -> m ()
cancelQueue Queue{..}
  = liftIO $ modifyMVar_ threadList (\ids -> mapM_ killThread ids >> return [])


-- | fork an IO action controlled by a quantity semaphore;
-- this allows throttling evaluations such that only a maximum number
-- of threads is running at a given time
forkQSem :: MonadIO m => QSem -> IO () -> m ThreadId
forkQSem qsem action
  =  liftIO $ forkIO $ bracket_ (waitQSem qsem) (signalQSem qsem) action
    
