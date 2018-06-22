{-# LANGUAGE OverloadedStrings #-}
module Codex.Tester.Sql (
  sqlTester,
  sqlSelectTester,
  sqlEditTester,
  sqlSchemaTester,
  ) where

import           Codex.Tester
import           Data.Text(Text)
import           Control.Exception
import           Control.Applicative


sqlTester :: Tester Result
sqlTester = sqlSelectTester <|> sqlEditTester <|> sqlSchemaTester


sqlSelectTester :: Tester Result
sqlSelectTester = tester "select" $ do
  Code lang src <- testCode
  guard (lang == "sql")
  meta <- testMetadata
  ---
  evaluator <- configured "language.sql.select.evaluator"
  confArgs <- getOptConfArgs "language.sql.select.args"
              [ ("-H", "host")
              , ("-P", "port")
              , ("-u", "user")
              , ("-p", "pass")
              ]
  metaArgs <- getOptMetaArgs
              [ ("-d", "db-name") ]
  answer <- liftIO (getSqlAnswer meta)
  withTemp "submit.sql" src $ \submittedFilePath -> do
    chmod readable submittedFilePath
    classify <$> unsafeExec evaluator
      (confArgs ++ metaArgs ++ ["-a", answer, "-S", submittedFilePath]) ""


sqlEditTester :: Tester Result
sqlEditTester = tester "edit" $ do
  Code lang src <- testCode
  guard (lang == "sql")
  ---
  meta <- testMetadata
  evaluator <- configured "language.sql.edit.evaluator"
  confArgs <- getOptConfArgs "language.sql.edit.args"
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
  answer <- liftIO (getSqlAnswer meta)
  withTemp "submit.sql" src $ \submittedFilePath -> do
    chmod readable submittedFilePath
    classify <$> unsafeExec evaluator
      (confArgs ++ metaArgs ++ ["-a", answer,"-S", submittedFilePath]) ""


sqlSchemaTester :: Tester Result
sqlSchemaTester = tester "schema" $ do
  Code lang src <- testCode
  guard (lang == "sql")
  meta <- testMetadata
  ---
  evaluator <- configured "language.sql.schema.evaluator"
  confArgs <- getOptConfArgs "language.sql.schema.args"
             [ ("-H", "host")
             , ("-P", "port")
             , ("-u", "user")
             , ("-p", "pass")
             , ("-D", "prefix")
             ]
  metaArgs <- getOptMetaArgs
             [ ("-i", "db-init-sql")
             , ("-I", "db-init-file")
             ]
  answer <- liftIO (getSqlAnswer meta)
  withTemp "submit.sql" src $ \submittedFilePath -> do
    chmod readable submittedFilePath
    classify <$> unsafeExec evaluator
      (confArgs ++ metaArgs ++ ["-a", answer, "-S", submittedFilePath]) ""


getOptConfArgs :: Text -> [(String, Text)] -> Tester [String]
getOptConfArgs prefix opts =
  concat <$> mapM optConfArg opts
  where
    optConfArg (opt, key) = do
      cnf <- maybeConfigured (prefix<>"."<>key)
      return $ maybe [] (\x -> [opt, x]) cnf


getOptMetaArgs :: [(String, String)] -> Tester [String]
getOptMetaArgs opts = do
  meta <- testMetadata
  let optMetaArg (opt,key) =
          maybe [] (\x -> [opt, x]) (lookupFromMeta key meta)
  return $ concatMap optMetaArg opts


classify :: (ExitCode, Text, Text) -> Result
classify (ExitFailure 100, stdout, _) = accepted stdout
classify (ExitFailure 101, stdout, _) = wrongAnswer stdout
classify (ExitFailure 102, stdout, _) = runtimeError stdout
classify (ExitFailure 103, stdout, _) = compileError stdout
classify (ExitFailure 104, stdout, _) = timeLimitExceeded stdout
classify (ExitFailure 105, stdout, _) = memoryLimitExceeded stdout
classify (_, stdout, stderr)          = miscError (stdout <> stderr)


getSqlAnswer :: Meta -> IO String
getSqlAnswer meta = do
  case lookupFromMeta "answer-sql" meta of
    Nothing ->
      throwIO (miscError "no sql-answer specified in metadata")
    Just answer ->
      return answer
