{-# LANGUAGE RecordWildCards #-}
{- 
-- Manage a thread pool for evaluation tasks
-}
module Codex.Tasks where

import           Control.Concurrent
import           Control.Exception (bracket_)


-- | a mutable list of threads for pending evaluations
type Tasks = MVar [ThreadId]

-- | for a thread locked with a semaphore
forkSingle :: QSem -> IO () -> IO ThreadId
forkSingle semph action
  = forkIO $ bracket_ (waitQSem semph) (signalQSem semph) action

-- | fork several threads with a semaphore
-- allows possible canceling
forkMany :: QSem -> Tasks -> [IO ()] -> IO ()
forkMany semph tasks actions =
  modifyMVar_ tasks $ \tids -> do
    mapM_ killThread tids
    tids' <- mapM (forkSingle semph) actions
    return tids'


makeTasks :: Int -> IO (QSem, Tasks)
makeTasks n = (,) <$> newQSem n <*> newMVar []
