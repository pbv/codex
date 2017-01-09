{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
------------------------------------------------------------------------------
-- | This module defines our application's state type and an alias for its
-- handler monad.
module Codex.Application where

------------------------------------------------------------------------------
import Control.Lens
import Control.Monad.State

import Snap.Snaplet
import Snap.Snaplet.Heist
import Snap.Snaplet.Auth
import Snap.Snaplet.Session
import Snap.Snaplet.SqliteSimple

import Control.Concurrent (MVar, ThreadId)
import Control.Concurrent.QSem

import Codex.Tester.Monad
import Codex.Tester.Result

------------------------------------------------------------------------------
data App = App
    { _heist :: Snaplet (Heist App)
    , _sess  :: Snaplet SessionManager
    , _auth  :: Snaplet (AuthManager App)
    , _db    :: Snaplet Sqlite
    , defaultTester :: Tester Result  -- default tester for exercises
    , evalThreads :: MVar [ThreadId]  -- list of pending evaluation threads
    , evalQS :: QSem        -- semaphore for "throttling" evaluation 
    }

makeLenses ''App

instance HasHeist App where
    heistLens = subSnaplet heist

instance HasSqlite (Handler App App) where
   getSqliteState = with db get

------------------------------------------------------------------------------
type Codex = Handler App App
------------------------------------------------------------------------------
