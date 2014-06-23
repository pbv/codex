{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Db where

import           Control.Monad
import           Data.Text (Text)
import qualified Data.Text as T

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
createTables conn =   mapM_ create tables
  -- Note: for a bigger app, you probably want to create a 'version'
  -- table too and use it to keep track of schema version and
  -- implement your schema upgrade procedure here.
  where create (tab,cmd) = do 
          schemaCreated <- tableExists conn tab
          unless schemaCreated $ S.execute_ conn (S.Query cmd)
      
tables :: [(String,Text)]
tables = [("submissions",  
           T.concat [ "CREATE TABLE submissions ("
                    , "id INTEGER PRIMARY KEY, "
                    , "user_id TEXT NOT NULL, "
                    , "problem_id TEXT NOT NULL, "
                    , "time TIMESTAMP NOT NULL, "
                    , "code TEXT NOT NULL, "
                    , "status TEXT NOT NULL, "
                    , "report TEXT NOT NULL)"])]





