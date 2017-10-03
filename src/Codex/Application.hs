{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-} -- Needed by
{-# LANGUAGE TypeFamilies      #-} -- web-routes
{-# LANGUAGE DeriveGeneric     #-} -- Needed to derive Generic
                                   -- for our URL data type

------------------------------------------------------------------------------
-- | This module defines our application's state type and an alias for its
-- handler monad.
module Codex.Application where

------------------------------------------------------------------------------
import Control.Lens

import Snap.Snaplet
import Snap.Snaplet.Heist
import Snap.Snaplet.Auth
import Snap.Snaplet.Session
import Snap.Snaplet.SqliteSimple
import Snap.Snaplet.Router.Types

import Control.Monad.State (get)

import Control.Concurrent (MVar, ThreadId)
import Control.Concurrent.QSem (QSem)

import Codex.Types

data AppUrl =
    Login
  | Logout
  | Page  [FilePath]
  | Report SubmitId
  | Files [FilePath]
  | Submissions
    deriving (Eq, Show, Read, Generic)

instance PathInfo AppUrl
------------------------------------------------------------------------------
data App = App
    { _heist :: Snaplet (Heist App)
    , _router :: Snaplet RouterState
    , _sess  :: Snaplet SessionManager
    , _auth  :: Snaplet (AuthManager App)
    , _db    :: Snaplet Sqlite
    , evalThreads   :: MVar [ThreadId]  -- ^ list of pending evaluation threads 
    , evalSem       :: QSem             -- ^ semaphore for "throttling" evaluations
   }

makeLenses ''App

instance HasHeist App where
    heistLens = subSnaplet heist

instance HasSqlite (Handler App App) where
   getSqliteState = with db get



-- You need to define a HasRouter instance for your app.
-- You must set type URL (Handler App App) to the URL
-- data type you defined above. The router in
-- `with router` is the lens for the @RouterState@ snaplet
-- you added to App.
instance HasRouter (Handler App App) where
    type URL (Handler App App) = AppUrl
    getRouterState = with router get
    

-- You also need to define a HasRouter instance for the
-- router snaplet. Once again, set type URL (Handler b
-- RouterState) to the data type you defined above.
instance HasRouter (Handler b RouterState) where
    type URL (Handler b RouterState) = AppUrl
    getRouterState = get
    

------------------------------------------------------------------------------
type Codex = Handler App App
------------------------------------------------------------------------------
