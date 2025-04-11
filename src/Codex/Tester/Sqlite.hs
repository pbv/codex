{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
--
-- | Test SQL queries using the SQLite interpreter
--
module Codex.Tester.Sqlite (
  sqliteTester
  ) where

import           Codex.Tester
import           Control.Applicative ((<|>))
import qualified Data.Text as T
import           Data.Text (Text)
import           Data.List (sort)

import qualified Text.Pandoc.Builder as P

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
  sqlite <- parseArgs =<< configured "language.sqlite.command" 
  answer <- metadataWithDefault "answer" ""
  assert (pure $ answer /= "") "missing SQL query answer in metadata"
  dir <- takeDirectory <$> testFilePath
  -- databases to try the query against
  pub_dbs <- globPatterns dir =<<
             metadataWithDefault "public-databases" []
  priv_dbs<- globPatterns dir =<<
             metadataWithDefault "private-databases" []
  assert (pure $ not $ null pub_dbs && null priv_dbs)
    "missing SQL public or private databases metadata"
  ordering <- metadataWithDefault "ignore-order" False
  let normalize = if ordering then sort else id
  rs1 <- liftIO $
         mapM (runQuery limits sqlite normalize answer query) pub_dbs
  rs2 <- liftIO $
         mapM (runQuery limits sqlite normalize answer query) priv_dbs
  return (tagWith Public (mconcat rs1) <> tagWith Private (mconcat rs2))

runQuery :: Limits -> [String] -> ([Text] -> [Text])
         -> Text -> Text -> FilePath
         -> IO Result
runQuery _      []               _         _           _         _ 
  = error "sqlite command not defined in config file"
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
             P.plain (P.text "Query passed for database " <> P.str dbname)
           else
             wrongAnswer $
             P.plain (P.text "Query failed for databse " <> P.str dbname)
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
        safeExec limits sqlcmd Nothing (sqlargs++["-readonly", "-column", "-header", db]) sql
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
         

------------------------------------------------------------------------
--
-- | Tester for updates
--
updateTester = tester "sqlite-update" $ do
  Code lang update <- testCode
  guard (lang == "sql")
  ---
  limits <- configLimits "language.sqlite.limits"
  sqlite <- parseArgs =<< configured "language.sqlite.command" 
  sqldiff<- parseArgs =<< configured "language.sqlite.diff" 
  answer <- metadataWithDefault "answer" ""
  assert (pure $ answer /= "") "missing SQL query answer in metadata"
  dir <- takeDirectory <$> testFilePath
  pub_dbs <- globPatterns dir =<< metadataWithDefault "databases" []
  priv_dbs <- globPatterns dir =<< metadataWithDefault "private-databases" []
  assert (pure $ not $ null pub_dbs && null priv_dbs)
        "no SQL databases specified in exercise metadata"
  rs1 <- liftIO $
         mapM (runUpdate limits sqlite sqldiff answer update) pub_dbs
  rs2 <- liftIO $
         mapM (runUpdate limits sqlite sqldiff answer update) priv_dbs
  return (tagWith Public (mconcat rs1) <> tagWith Private (mconcat rs2))


runUpdate :: Limits -> [String] -> [String] -> Text -> Text -> FilePath
          -> IO Result
runUpdate  _     []            _               _      _      _  =
  error "sqlite command not defined in config file"
runUpdate  _     _            []               _      _      _  =
  error "sqldiff command not defined in config file"
runUpdate limits (sqlite:args) (sqldiff:args') answer update db = do
  withTempFile "expected.db" db $ \expected ->
    withTempFile "observed.db" db $ \observed -> do
      r1 <- runSql answer expected
      r2 <- runSql update observed
      case (r1, r2) of
        (Left msg, _) ->
          return $ runtimeError $ P.codeBlock msg
        (_, Left msg) ->
          return $ runtimeError $ P.codeBlock msg
        (Right _, Right _) -> do
          r3 <- runDiff observed expected
          case r3 of
            Left msg ->
              return $ runtimeError $ P.codeBlock msg
            Right stdout ->
              let dbname = T.pack (takeFileName db)
              in 
              return $
              if T.null stdout then
                accepted $
                P.plain $ P.text $ "Update checked for database " <> dbname
              else
                wrongAnswer 
                ((P.plain $ P.text
                  ("Incorrect update for database " <> dbname))
                 <>
                 P.codeBlock stdout)
  where
    runSql :: Text -> FilePath -> IO (Either Text ())
    runSql sql db = do
      (exitCode, _stdout, stderr) <-
        safeExec limits sqlite Nothing (args ++ [db]) sql
      return $ case exitCode of
        ExitFailure _ ->
          Left stderr
        ExitSuccess ->
          if match "Error" stderr then
            Left stderr
          else
            Right ()

    runDiff :: FilePath -> FilePath -> IO (Either Text Text)
    runDiff db1 db2 = do
      (exitCode, stdout, stderr) <-
        safeExec limits sqldiff Nothing (args' ++ [db1, db2]) ""
      return $ case exitCode of
        ExitSuccess ->
          if match "Error" stderr
          then Left stderr
          else Right stdout
        ExitFailure _ ->
          Left stderr


