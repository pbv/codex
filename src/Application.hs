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
data Pythondo = Pythondo
    { _heist :: Snaplet (Heist Pythondo)
    , _sess  :: Snaplet SessionManager
    , _auth  :: Snaplet (AuthManager Pythondo)
    , _db    :: Snaplet Sqlite
    -- , _config :: Config                  -- configurator handle
    , _sandbox :: Sandbox                -- safeexec configuration
    , _ldapConf :: LdapConf              -- LDAP configuration
    , _printout :: Printout              -- printout configuration
    , _ekg     :: Maybe Server           -- optional EKG monitoring server
    }

makeLenses ''Pythondo

instance HasHeist Pythondo where
    heistLens = subSnaplet heist

instance HasSqlite (Handler Pythondo Pythondo) where
   getSqliteState = with db get
   
------------------------------------------------------------------------------
type AppHandler = Handler Pythondo Pythondo
------------------------------------------------------------------------------
