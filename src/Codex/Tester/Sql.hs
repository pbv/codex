{-# LANGUAGE OverloadedStrings #-}
module Codex.Tester.Sql (
  sqlTester,
  sqlSelectTester,
  sqlEditTester,
  sqlSchemaTester,
  ) where

import           Codex.Tester
import           Data.Text(Text)
import qualified Data.Text as T
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
  evaluator <- configured "language.sql.evaluator.select"
  confArgs <- getOptConfArgs "language.sql.args"
              [ ("-H", "host")
              , ("-P", "port")
              , ("-u", "user_guest")
              , ("-p", "pass_guest")
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
classify (ExitSuccess, stdout, _)
  | match "Accepted" stdout              = accepted (dropFirstLn stdout)
  | match "Wrong Answer" stdout          = wrongAnswer (dropFirstLn stdout)
  | match "Runtime Error" stdout         = runtimeError (dropFirstLn stdout)
  | match "Compile Error" stdout         = compileError (dropFirstLn stdout)
  | match "Time Limit Exceeded" stdout   = timeLimitExceeded (dropFirstLn stdout)
  | match "Memory Limit Exceeded" stdout = memoryLimitExceeded (dropFirstLn stdout)
  where
    dropFirstLn = T.dropWhile (/='\n')
classify (_, stdout, stderr)             = miscError (stdout <> stderr)


getSqlAnswer :: Meta -> IO String
getSqlAnswer meta = do
  case lookupFromMeta "answer-sql" meta of
    Nothing ->
      throwIO (miscError "no sql-answer specified in metadata")
    Just answer ->
      return answer
