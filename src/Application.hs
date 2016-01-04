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
-- import Language
-- import SafeExec 

------------------------------------------------------------------------------
data App = App
    { _heist :: Snaplet (Heist App)
    , _sess  :: Snaplet SessionManager
    , _auth  :: Snaplet (AuthManager App)
    , _db    :: Snaplet Sqlite
    , pythonConf :: PythonConf          -- python configuration
    , haskellConf :: HaskellConf
    , ldapConf :: LdapConf              -- LDAP configuration
    , printConf :: PrintConf            -- printout configuration
    , ekg :: Maybe Server           -- optional EKG monitoring server
    }

makeLenses ''App

instance HasHeist App where
    heistLens = subSnaplet heist

instance HasSqlite (Handler App App) where
   getSqliteState = with db get
   
------------------------------------------------------------------------------
type AppHandler = Handler App App
------------------------------------------------------------------------------
