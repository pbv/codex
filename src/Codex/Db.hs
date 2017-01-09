{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
{-
  Initialize backend SQLite database tables
-}
module Codex.Db where

import           Control.Monad
import qualified Data.Map as Map
import           Data.Map.Syntax

import qualified Database.SQLite.Simple as S
import           Snap.Snaplet.SqliteSimple


tableExists :: S.Connection -> String -> IO Bool
tableExists conn tblName = do
  r <- S.query conn "SELECT name FROM sqlite_master WHERE type='table' AND name=?" (Only tblName)
  return $ case r of
    [Only (_ :: String)] -> True
    _ -> False

-- | Create the necessary database tables, if not already initialized.
{-
createTables :: S.Connection -> IO ()
createTables conn = do
  schemaCreated <- tableExists conn "submissions"
  unless schemaCreated $ mapM_ (S.execute_ conn) initCmds
-}
createTables :: S.Connection -> IO ()
createTables conn =
    forM_ initCmds $ \(table,cmds) -> do
      c <- tableExists conn table
      unless c $ mapM_ (S.execute_ conn) cmds
      -- Note: for a bigger app, you probably want to create a 'version'
      -- table too and use it to keep track of schema version and
      -- implement your schema upgrade procedure here.


--  | initialization SQL commands for each Db table
initCmds :: [(String, [S.Query])]
initCmds = either (error.show) Map.assocs $ runMap $ do
  "events"  ## ["CREATE TABLE events (id INTEGER PRIMARY KEY, \
                 \name TEXT NOT NULL, time TIMESTAMP NOT NULL)"]
  "submissions" ## ["CREATE TABLE submissions (\
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
