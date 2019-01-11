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
  Code lang src <- askSubmitted
  guard (lang == "sql")
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
  answer <- getSqlAnswer
  withTemp "answer.sql" (T.pack answer) $ \answerFilePath ->
    withTemp "submit.sql" src $ \submittedFilePath ->
      classify <$> unsafeExec evaluator
        (confArgs ++ metaArgs ++ ["-A", answerFilePath,"-S", submittedFilePath]) ""


sqlEditTester :: Tester Result
sqlEditTester = tester "edit" $ do
  Code lang src <- askSubmitted
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
  Code lang src <- askSubmitted
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
