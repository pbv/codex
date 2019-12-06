{-# LANGUAGE OverloadedStrings #-}
--
-- | Test SQL queries using SQLite interpreter
--
module Codex.Tester.Sql (
  sqliteTester,
  ) where

import           Codex.Tester
import           Data.Maybe (fromMaybe)
import qualified Data.Text as T
import           Data.List (sort)
import           Control.Exception (throwIO, catch)
import           System.FilePath (takeFileName)


sqliteTester :: Tester Result
sqliteTester = tester "sqlite" $ do
  Code lang query <- testCode
  guard (lang == "sql")
  ---
  limits <- configLimits "language.sqlite.limits"
  cmdline <- configured "language.sqlite.command" >>= parseArgs
  answer <- fromMaybe "" <$> metadata "answer"
  assert (pure $ answer /= "") "missing SQL query answer in metadata"
  dir <- takeDirectory <$> testFilePath
  inpatts <- map (dir</>) . fromMaybe [] <$> metadata "databases"
  assert (pure $ not $ null inpatts) "missing SQL databases in metadata"
  inputs <- concat <$> globPatterns inpatts
  ordering <- fromMaybe False <$> metadata "ignore-order"
  let normalize = case ordering of
        False -> T.strip
        True -> T.unlines . sort . T.lines
  liftIO (runTests limits cmdline answer query normalize inputs
           `catch` return)

runTests _      []               _      _     _         _       =
  throwIO $ userError "no SQLite command in config file"
runTests limits (sqlcmd:sqlargs) answer query normalize inputs  = do
    loop 1 inputs
  where
    total = length inputs
    runQuery db sql = do
      (exitCode, stdout, stderr) <-
        safeExec limits sqlcmd Nothing (sqlargs++["-init", db]) sql
      case exitCode of
        ExitSuccess -> return stdout
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
      

{-


-- sqlTester :: Tester Result
-- sqlTester = sqlSelectTester <|> sqlEditTester <|> sqlSchemaTester


sqlEditTester :: Tester Result
sqlEditTester = tester "edit" $ do
  Code lang src <- testCode
  guard (lang == "sql")
  ---
  evaluator <- configured "language.sql.evaluator.edit"
  confArgs <- getOptConfArgs "language.sql.args"
             [ ("-H", "host")
             , ("-P", "port")
             , ("-uS", "user_schema")
             , ("-pS", "pass_schema")
             , ("-uE", "user_edit")
             , ("-pE", "pass_edit")
             , ("-D", "prefix")
             ]
  metaArgs <- getOptMetaArgs
             [ ("-i", "db-init-sql")
             , ("-I", "db-init-file")
             ]
  answer <- getSqlAnswer
  withTemp "answer.sql" (T.pack answer) $ \answerFilePath ->
    withTemp "submit.sql" src $ \submittedFilePath ->
      classify <$> unsafeExec evaluator
        (confArgs ++ metaArgs ++ ["-A", answerFilePath,"-S", submittedFilePath]) ""


sqlSchemaTester :: Tester Result
sqlSchemaTester = tester "schema" $ do
  Code lang src <- testCode
  guard (lang == "sql")
  ---
  evaluator <- configured "language.sql.evaluator.schema"
  confArgs <- getOptConfArgs "language.sql.args"
             [ ("-H", "host")
             , ("-P", "port")
             , ("-u", "user_schema")
             , ("-p", "pass_schema")
             , ("-D", "prefix")
             ]
  metaArgs <- getOptMetaArgs
             [ ("-i", "db-init-sql")
             , ("-I", "db-init-file")
             ]
  answer <- getSqlAnswer
  withTemp "answer.sql" (T.pack answer) $ \answerFilePath ->
    withTemp "submit.sql" src $ \submittedFilePath ->
      classify <$> unsafeExec evaluator
        (confArgs ++ metaArgs ++ ["-A", answerFilePath,"-S", submittedFilePath]) ""


getOptConfArgs :: Text -> [(String, Text)] -> Tester [String]
getOptConfArgs prefix opts =
  concat <$> mapM optConfArg opts
  where
    optConfArg (opt, key) = do
      cnf <- maybeConfigured (prefix<>"."<>key)
      return $ maybe [] (\x -> [opt, x]) cnf


getOptMetaArgs :: [(String, String)] -> Tester [String]
getOptMetaArgs opts
  = concat <$> mapM optMetaArg opts
  where
    optMetaArg (opt,key) = maybe [] (\x -> [opt, x]) <$> metadata key




classify :: (ExitCode, Text, Text) -> Result
classify (ExitSuccess, stdout, _)
  | T.isPrefixOf "Accepted" stdout              = accepted (dropFirstLn stdout)
  | T.isPrefixOf "Wrong Answer" stdout          = wrongAnswer (dropFirstLn stdout)
  | T.isPrefixOf "Runtime Error" stdout         = runtimeError (dropFirstLn stdout)
  | T.isPrefixOf "Compile Error" stdout         = compileError (dropFirstLn stdout)
  | T.isPrefixOf "Time Limit Exceeded" stdout   = timeLimitExceeded (dropFirstLn stdout)
  | T.isPrefixOf "Memory Limit Exceeded" stdout = memoryLimitExceeded (dropFirstLn stdout)
  where
    dropFirstLn = T.dropWhile (/='\n')
classify (_, stdout, stderr)                    = miscError (stdout <> stderr)


getSqlAnswer :: Tester String
getSqlAnswer  = do
  opt <- metadata "answer-sql"
  case opt of
    Nothing ->
      liftIO $ throwIO $ miscError "missing answer-sql in metadata"
    Just answer ->
      return answer
-}
