{-# LANGUAGE OverloadedStrings #-}
--
-- | Test SQL queries using SQLite interpreter
--
module Codex.Tester.Sql (
  sqliteTester
  ) where

import           Codex.Tester
import           Control.Applicative (empty,(<|>))
import           Data.Maybe (fromMaybe)
import qualified Data.Text as T
import           Data.List (sort)
import           Control.Exception (throwIO, catch)

sqliteTester :: Tester Result
sqliteTester = empty -- queryTester <|> updateTester

{-
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
  liftIO (runQueries limits sqlite answer query normalize (map T.pack inputs)
           `catch` return)

runQueries :: Limits -> [FilePath] -> T.Text -> T.Text -> (T.Text -> T.Text) -> [T.Text] -> IO Result
runQueries _      []               _      _     _         _       =
  throwIO $ userError "no SQLite command in config file"
runQueries limits (sqlcmd:sqlargs) answer query normalize inputs  =
    loop 1 inputs
  where
    total = length inputs
    runQuery db sql = do
      (exitCode, stdout, stderr) <-
        safeExec limits sqlcmd Nothing (map T.pack sqlargs++["-init", db]) sql
      case exitCode of
        ExitSuccess ->
          -- NB: SQLite can exit with zero code in many errors,
          -- so we need to check stderr
          if match "Error" stderr then
            throwIO $ runtimeError stderr
            else return stdout
        ExitFailure _ -> throwIO $ runtimeError stderr
    ---
    loop :: Int -> [T.Text] -> IO Result
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
                             T.pack (takeFileName (T.unpack db))
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
  liftIO (runUpdates limits sqlite sqldiff answer update (map T.pack inputs)
           `catch` return)

runUpdates :: Limits -> [FilePath] -> [FilePath] -> T.Text -> T.Text -> [T.Text] -> IO Result
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
        safeExec limits sqldiff Nothing (map T.pack args' ++ [db1, db2]) ""
      case exitCode of
        ExitSuccess -> if match "Error" stderr
                       then throwIO $ runtimeError stderr
                            else return stdout
        ExitFailure _ -> throwIO $ runtimeError stderr
    runUpdate db sql file = do
      (exitCode, _, stderr) <-
        safeExec limits sqlite Nothing (map T.pack args++["-init", db, file]) sql
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
                  runUpdate db answer (T.pack expectf)
                  runUpdate db update (T.pack observef)
                  runDiff (T.pack observef) (T.pack expectf)
      if T.null stdout then loop (n+1) rest
        else throwIO $ wrongAnswer stdout
-}
