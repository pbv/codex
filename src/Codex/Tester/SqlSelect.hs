{-# LANGUAGE OverloadedStrings #-}
module Codex.Tester.SqlSelect (
  sqlSelectTester
  ) where

import           Codex.Tester
import           Data.Text(Text)


sqlSelectTester :: PageInfo -> Code -> Test Result
sqlSelectTester (PageInfo _ meta) (Code lang src) = do
  guard (lang == "sql")
  guard (tester meta == Just "select")
  ---
  evaluator <- configured "language.sql.select.evaluator"
  let db_host = maybe [] (\x -> ["--host", x]) (lookupFromMeta "db-host" meta)
  let db_user = maybe [] (\x -> ["--user", x]) (lookupFromMeta "db-user" meta)
  let db_name = maybe [] (\x -> ["--db", x]) (lookupFromMeta "db-name" meta)
  let sql_answer = maybe "" id $ lookupFromMeta "sql-answer" meta
  withTemp "submit.sql" src $ \submittedFilePath -> do
    chmod readable submittedFilePath
    classify <$> unsafeExec evaluator
      (db_host ++ db_user ++ db_name ++ ["--query2-is-file", sql_answer, submittedFilePath]) ""


classify :: (ExitCode, Text, Text) -> Result
classify (_, stdout, stderr)
  | match "Accepted" stderr            = accepted stdout
  | match "Wrong Answer" stderr        = wrongAnswer stdout
  | match "Runtime Error" stderr       = runtimeError stdout
  | match "Compile Error" stderr       = compileError stdout
--  | match "Time Limit" stderr          = timeLimitExceeded stdout
--  | match "Memory Limit" stderr        = memoryLimitExceeded stdout
  | otherwise                          = miscError (stdout <> stderr)
