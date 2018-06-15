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
  let args = concatMap (optArg meta)
             [ ("-H", "db-host")
             , ("-P", "db-port")
             , ("-u", "db-user")
             , ("-p", "db-pass")
             , ("-d", "db-name")
             ]
  answer <- liftIO (getSqlAnswer meta)
  withTemp "submit.sql" src $ \submittedFilePath -> do
    chmod readable submittedFilePath
    classify <$> unsafeExec evaluator
      (args ++ ["-a", answer, "-S", submittedFilePath]) ""


sqlEditTester :: Tester Result
sqlEditTester = tester "edit" $ do
  Code lang src <- testCode
  guard (lang == "sql")
  ---
  meta <- testMetadata
  evaluator <- configured "language.sql.edit.evaluator"
  let args = concatMap (optArg meta)
             [ ("-H", "db-host")
             , ("-P", "db-port")
             , ("-uS", "db-user-schema")
             , ("-pS", "db-pass-schema")
             , ("-uE", "db-user-edit")
             , ("-pE", "db-pass-edit")
             , ("-D", "db-prefix")
             , ("-i", "db-init-sql")
             , ("-I", "db-init-file")
             ]
  answer <- liftIO (getSqlAnswer meta)
  withTemp "submit.sql" src $ \submittedFilePath -> do
    chmod readable submittedFilePath
    classify <$> unsafeExec evaluator
      (args ++ ["-a", answer,"-S", submittedFilePath]) ""


sqlSchemaTester :: Tester Result
sqlSchemaTester = tester "schema" $ do
  Code lang src <- testCode
  guard (lang == "sql")
  meta <- testMetadata
  ---
  evaluator <- configured "language.sql.schema.evaluator"
  let args = concatMap (optArg meta)
             [ ("-H", "db-host")
             , ("-P", "db-port")
             , ("-u", "db-user")
             , ("-p", "db-pass")
             , ("-D", "db-prefix")
             , ("-i", "db-init-sql")
             , ("-I", "db-init-file")
             ]
  answer <- liftIO (getSqlAnswer meta)
  withTemp "submit.sql" src $ \submittedFilePath -> do
    chmod readable submittedFilePath
    classify <$> unsafeExec evaluator
      (args ++ ["-a", answer, "-S", submittedFilePath]) ""


optArg :: Meta -> (String, String) -> [String]
optArg meta (opt,key)
  = maybe [] (\x -> [opt, x]) (lookupFromMeta key meta)


classify :: (ExitCode, Text, Text) -> Result
classify (_, stdout, stderr)
  | match "Accepted" stderr            = accepted stdout
  | match "Wrong Answer" stderr        = wrongAnswer stdout
  | match "Runtime Error" stderr       = runtimeError stdout
  | match "Compile Error" stderr       = compileError stdout
  | match "Time Limit" stderr          = timeLimitExceeded stdout
  | match "Memory Limit" stderr        = memoryLimitExceeded stdout
  | otherwise                          = miscError (stderr <> stdout)


getSqlAnswer :: Meta -> IO String
getSqlAnswer meta = do
  case lookupFromMeta "answer-sql" meta of
    Nothing ->
      throwIO (miscError "no sql-answer specified in metadata")
    Just answer ->
      return answer
