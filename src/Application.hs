{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
------------------------------------------------------------------------------
-- | This module defines our application's state type and an alias for its
-- handler monad.
module Application where

------------------------------------------------------------------------------
import Control.Lens
import Control.Monad.State

import Snap.Snaplet
import Snap.Snaplet.Heist
import Snap.Snaplet.Auth
import Snap.Snaplet.Session
import Snap.Snaplet.SqliteSimple

-- import Data.Configurator
-- import Data.Configurator.Types
import System.Remote.Monitoring

import Types

------------------------------------------------------------------------------
data App = App
    { _heist :: Snaplet (Heist App)
    , _sess  :: Snaplet SessionManager
    , _auth  :: Snaplet (AuthManager App)
    , _db    :: Snaplet Sqlite
    -- , _config :: Config                  -- configurator handle
    , _sandbox :: Sandbox                -- safeexec configuration
    , _ldapConf :: LdapConf              -- LDAP configuration
    , _printConf :: PrintConf            -- printout configuration
    , _ekg     :: Maybe Server           -- optional EKG monitoring server
    }

makeLenses ''App

instance HasHeist App where
    heistLens = subSnaplet heist

instance HasSqlite (Handler App App) where
   getSqliteState = with db get
   
------------------------------------------------------------------------------
type Pythondo = Handler App App
------------------------------------------------------------------------------
