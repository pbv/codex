{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
--
-- | Test SQL queries using the SQLite interpreter
--
module Codex.Tester.Sqlite (
  sqliteTester
  ) where

import           Codex.Tester
-- import           Control.Applicative (empty,(<|>))
import           Data.Maybe (fromMaybe)
import qualified Data.Text as T
import           Data.Text (Text)
import           Data.List (sort)

import qualified Text.Pandoc.Builder as P

sqliteTester :: Tester Result
sqliteTester = queryTester  -- <|> updateTester


--
-- | Tester for queries
--
queryTester :: Tester Result
queryTester = tester "sqlite-query" $ do
  Code lang query <- testCode
  guard (lang == "sql")
  ---
  limits <- configLimits "language.sqlite.limits"
  sqlite <- parseArgs =<< configured "language.sqlite.command" 
  answer <- fromMaybe "" <$> metadata @Text "answer"
  assert (pure $ answer /= "") "missing SQL query answer in metadata"
  dir <- takeDirectory <$> testFilePath
  -- databases to try the query against
  pub_dbs <- globPatterns dir =<< metadataWithDefault "databases" []
  priv_dbs<- globPatterns dir =<< metadataWithDefault "private-databases" []
  assert (pure $ not $ null pub_dbs && null priv_dbs)
    "missing SQL public or private databases metadata"
  ordering <- metadataWithDefault "ignore-order" False
  let normalize = if ordering then sort else id
  rs1 <- liftIO $
         mapM (runQuery limits sqlite normalize answer query) pub_dbs
  rs2 <- liftIO $
         mapM (runQuery limits sqlite normalize answer query) priv_dbs
  return (tagWith Public (mconcat rs1) <> tagWith Private (mconcat rs2))

runQuery :: Limits -> [FilePath] -> ([Text] -> [Text])
         -> Text -> Text -> FilePath
         -> IO Result
runQuery _ [] _ _ _ _ 
  = error "not sqlite command defined"
runQuery limits (sqlcmd:sqlargs) normalize answer_sql query_sql db = do
  expected <- runSqlite answer_sql db 
  obtained <- runSqlite query_sql db
  return $
    case (expected, obtained) of
      (Left msg, _) ->
        runtimeError $ P.codeBlock msg
      (_, Left msg) ->
        runtimeError $ P.codeBlock msg
      (Right out1, Right out2) ->
        let dbname = T.pack (takeFileName db)
        in if normalize out1 == normalize out2 then
             accepted $
             P.plain (P.text "Query passed for " <> P.str dbname)
           else
             wrongAnswer $
             P.plain (P.text "Query failed for " <> P.str dbname)
             <>
             P.simpleTable [ P.plain (P.text "Expected"),
                             P.plain (P.text "Obtained") ]
             [ [ trimLines maxOutput out1
               , trimLines maxOutput out2 ]
             ]
  where
    runSqlite :: Text -> FilePath -> IO (Either Text [Text])
    runSqlite sql db = do
      (exitCode, stdout, stderr) <-
        safeExec limits sqlcmd Nothing (sqlargs++["-readonly", "-column", "-noheader", db]) sql
      return $ case exitCode of
        ExitFailure _ ->
          Left stderr
        ExitSuccess ->
          -- NB: SQLite can exit with zero code in many errors,
          -- so we need to check stderr
          if match "Error" stderr then
            Left stderr
          else
            Right (T.lines stdout)


maxOutput :: Int
maxOutput = 100

-- | trim output a maximum number of lines
trimLines :: Int -> [Text] -> P.Blocks
trimLines n ls
  = if length ls <= n then
         P.codeBlock (T.unlines ls)
       else
         P.codeBlock (T.unlines $ take n ls)
         <>
         P.plain (P.emph $ P.text "Output too long (truncated)")
         


{-
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
