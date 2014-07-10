{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
{- 
  Initialize backend SQLite database tables 
-}
module Db where

import           Control.Monad
import           Data.Text (Text)
import qualified Data.Text as T

import qualified Database.SQLite.Simple as S
import           Snap.Snaplet.SqliteSimple

import           Problem (Problem(..), getProblems)

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
            \problem_id TEXT NOT NULL, \
            \time TIMESTAMP NOT NULL, \
            \code TEXT NOT NULL, \
            \status TEXT NOT NULL, \
            \report TEXT NOT NULL)",
            "CREATE TABLE problemtags (\
          \  id INTEGER PRIMARY KEY, \
           \ problem_id TEXT NOT NULL, \
           \ tag TEXT NOT NULL)",
            "CREATE INDEX user_index ON submissions(user_id)" 
           ]



-- remove all problem tags and insert new ones
updateProblems :: S.Connection -> [Problem t] -> IO ()
updateProblems conn probs = do
  S.execute_ conn "DELETE FROM problemtags"
  sequence_ [ insert pid tag | p <- probs, let pid = probID p, tag <- probTags p]
  where
    insert pid tag 
      = S.execute conn "INSERT INTO problemtags(problem_id, tag) VALUES(?,?)" (pid,tag)
