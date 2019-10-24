{-# LANGUAGE RecordWildCards #-}
--
-- Manage asynchronous evaluation tasks
--
module Codex.Tasks
  ( module Control.Concurrent,
    withQSem,
    PendingQ,
  ) where

import           Control.Monad.IO.Class
import           Control.Concurrent
import           Control.Exception (bracket_)

-- | a queue for pending evaluations
type PendingQ = MVar [ThreadId]

-- | bracket an IO action under a quantity semaphore
withQSem ::  MonadIO m => QSem -> IO () -> m ()
withQSem qsem action
  =  liftIO $ bracket_ (waitQSem qsem) (signalQSem qsem) action 







