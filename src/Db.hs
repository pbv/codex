{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, RecordWildCards #-}
{- 
  Initialize backend SQLite database tables 
-}
module Db where

import           Control.Monad

import qualified Database.SQLite.Simple as S
import           Snap.Snaplet.SqliteSimple


tableExists :: S.Connection -> String -> IO Bool
tableExists conn tblName = do
  r <- S.query conn "SELECT name FROM sqlite_master WHERE type='table' AND name=?" (Only tblName)
  case r of
    [Only (_ :: String)] -> return True
    _ -> return False

-- | Create the necessary database tables, if not already initialized.
createTables :: S.Connection -> IO ()
createTables conn = do
  schemaCreated <- tableExists conn "submissions"
  unless schemaCreated $ mapM_ (S.execute_ conn) initCmds
  -- Note: for a bigger app, you probably want to create a 'version'
  -- table too and use it to keep track of schema version and
  -- implement your schema upgrade procedure here.
      

initCmds :: [S.Query]
initCmds = ["CREATE TABLE submissions (\
            \id INTEGER PRIMARY KEY, \
            \user_id TEXT NOT NULL, \
            \path TEXT NOT NULL, \
            \received TIMESTAMP NOT NULL, \
            \language TEXT NOT NULL, \
            \code TEXT NOT NULL, \
            \class TEXT NOT NULL, \
            \message TEXT NOT NULL, \
            \timing TEXT NOT NULL)",
            "CREATE INDEX user_index ON submissions(user_id)"
           ]




