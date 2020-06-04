{-# LANGUAGE OverloadedStrings #-}
--
-- | Test SQL queries using SQLite interpreter
--
module Codex.Tester.Sql (
  sqliteTester
  ) where

import           Codex.Tester
import           Control.Applicative ((<|>))
import           Data.Maybe (fromMaybe)
import qualified Data.Text as T
import           Data.List (sort)
import           Control.Exception (throwIO, catch)
import           System.FilePath (takeFileName)

sqliteTester :: Tester Result
sqliteTester = queryTester <|> updateTester


--
-- | Tester for queries
--
queryTester :: Tester Result
queryTester = tester "sqlite-query" $ do
  Code lang query <- testCode
  guard (lang == "sql")
  ---
  limits <- configLimits "language.sqlite.limits"
  sqlite <- configured "language.sqlite.command" >>= parseArgs
  answer <- fromMaybe "" <$> metadata "answer"
  assert (pure $ answer /= "") "missing SQL query answer in metadata"
  dir <- takeDirectory <$> testFilePath
  inputs <- globPatterns dir =<< metadataWithDefault "databases" []
  assert (pure $ not $ null inputs) "missing SQL databases metadata"
  ordering <- metadataWithDefault "ignore-order" False
  let normalize = if ordering then T.unlines . sort . T.lines else T.strip
  liftIO (runQueries limits sqlite answer query normalize inputs
           `catch` return)

runQueries _      []               _      _     _         _       =
  throwIO $ userError "no SQLite command in config file"
runQueries limits (sqlcmd:sqlargs) answer query normalize inputs  =
    loop 1 inputs
  where
    total = length inputs
    runQuery db sql = do
      (exitCode, stdout, stderr) <-
        safeExec limits sqlcmd Nothing (sqlargs++["-init", db]) sql
      case exitCode of
        ExitSuccess ->
          -- NB: SQLite can exit with zero code in many errors,
          -- so we need to check stderr
          if match "Error" stderr then
            throwIO $ runtimeError stderr
            else return stdout
        ExitFailure _ -> throwIO $ runtimeError stderr
    ---
    loop _ []
      = return $ accepted $ "Passed " <> T.pack (show total) <> " tests"
    loop n (db : rest) = do
      obtained <- runQuery db query
      expected <- runQuery db answer
      if normalize obtained == normalize expected then
        loop (n+1) rest
        else
        return $ wrongAnswer $
                 T.unlines [ "Test " <> T.pack (show n) <> " / " <>
                             T.pack (show total) <>
                             " using database " <>
                             T.pack (takeFileName db)
                           , ""
                           , "EXPECTED:"
                           , expected
                           , ""
                           , "OBTAINED:"
                           , obtained
                           ]



--
-- | Tester for updates
--
updateTester = tester "sqlite-update" $ do
  Code lang update <- testCode
  guard (lang == "sql")
  ---
  limits <- configLimits "language.sqlite.limits"
  sqlite <- configured "language.sqlite.command" >>= parseArgs
  sqldiff<- configured "language.sqlite.diff" >>= parseArgs
  answer <- metadataWithDefault "answer" ""
  assert (pure $ answer /= "") "missing SQL query answer in metadata"
  dir <- takeDirectory <$> testFilePath
  inputs <- globPatterns dir =<< metadataWithDefault "databases" []
  assert (pure $ not $ null inputs) "missing SQL databases in metadata"
  liftIO (runUpdates limits sqlite sqldiff answer update inputs
           `catch` return)

runUpdates  _      []            _               _      _      _      =
  throwIO $ userError "no SQLite command in config file"
runUpdates  _      _            []               _      _      _      =
  throwIO $ userError "no SQLite diff command in config file"

runUpdates limits (sqlite:args) (sqldiff:args') answer update inputs =
  loop 1 inputs
  where
    total = length inputs
    runDiff db1 db2 = do
      (exitCode, stdout, stderr) <-
        safeExec limits sqldiff Nothing (args' ++ [db1, db2]) ""
      case exitCode of
        ExitSuccess -> if match "Error" stderr
                       then throwIO $ runtimeError stderr
                            else return stdout
        ExitFailure _ -> throwIO $ runtimeError stderr
    runUpdate db sql file = do
      (exitCode, _, stderr) <-
        safeExec limits sqlite Nothing (args++["-init", db, file]) sql
      case exitCode of
        ExitSuccess ->
          -- NB: SQLite can exit with zero code in many errors,
          -- so we need to check stderr
          when (match "Error" stderr) $
            throwIO $ runtimeError ("runUpdate: " <> stderr)
        ExitFailure _ -> throwIO $ runtimeError ("runUpdate: "<> stderr)
    ---
    loop _ []
      = return $ accepted $ "Passed " <> T.pack (show total) <> " tests"
    loop n (db : rest) = do
      stdout <- withTemp "expected.db" "" $ \expectf ->
                  withTemp "observed.db" "" $ \observef -> do
                  chmod (readable . writeable) expectf
                  chmod (readable . writeable) observef
                  runUpdate db answer expectf
                  runUpdate db update observef
                  runDiff observef expectf
      if T.null stdout then loop (n+1) rest
        else throwIO $ wrongAnswer stdout
