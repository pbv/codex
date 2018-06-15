{-# LANGUAGE OverloadedStrings #-}
module Codex.Tester.SqlSelect (
  sqlSelectTester
  ) where

import           Codex.Tester
import           Data.Text(Text)


sqlSelectTester :: Tester Result
sqlSelectTester = tester "select" $ do
  Code lang src <- testCode
  guard (lang == "sql")
  meta <- testMetadata
  ---
  evaluator <- configured "language.sql.select.evaluator"
  let db_host = maybe [] (\x -> ["-H", x]) (lookupFromMeta "db-host" meta)
  let db_port = maybe [] (\x -> ["-P", x]) (lookupFromMeta "db-port" meta)
  let db_user = maybe [] (\x -> ["-u", x]) (lookupFromMeta "db-user" meta)
  let db_passwd = maybe [] (\x -> ["-p", x]) (lookupFromMeta "db-pass" meta)
  let db_name = maybe [] (\x -> ["-d", x]) (lookupFromMeta "db-name" meta)
  let answer_sql = maybe "" id $ lookupFromMeta "answer-sql" meta
  withTemp "submit.sql" src $ \submittedFilePath -> do
    chmod readable submittedFilePath
    classify <$> unsafeExec evaluator
      (db_host ++ db_port ++ db_user ++ db_passwd ++ db_name ++ ["-a", answer_sql, "-S", submittedFilePath]) ""


classify :: (ExitCode, Text, Text) -> Result
classify (_, stdout, stderr)
  | match "Accepted" stderr            = accepted stdout
  | match "Wrong Answer" stderr        = wrongAnswer stdout
  | match "Runtime Error" stderr       = runtimeError stdout
  | match "Compile Error" stderr       = compileError stdout
  | match "Time Limit" stderr          = timeLimitExceeded stdout
  | match "Memory Limit" stderr        = memoryLimitExceeded stdout
  | otherwise                          = miscError (stderr <> stdout)
