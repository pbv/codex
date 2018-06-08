{-# LANGUAGE OverloadedStrings #-}
module Codex.Tester.SqlEdit (
  sqlEditTester
  ) where

import           Codex.Tester
import           Data.Text(Text)


sqlEditTester :: PageInfo -> Code -> Test Result
sqlEditTester (PageInfo _ meta) (Code lang src) = do
  guard (lang == "sql")
  guard (tester meta == Just "edit")
  ---
  evaluator <- configured "language.sql.edit.evaluator"
  let db_host = maybe [] (\x -> ["-H", x]) (lookupFromMeta "db-host" meta)
  let db_port = maybe [] (\x -> ["-P", x]) (lookupFromMeta "db-port" meta)
  let db_userS = maybe [] (\x -> ["-uS", x]) (lookupFromMeta "db-user-schema" meta)
  let db_passwdS = maybe [] (\x -> ["-pS", x]) (lookupFromMeta "db-pass-schema" meta)
  let db_userE = maybe [] (\x -> ["-uE", x]) (lookupFromMeta "db-user-edit" meta)
  let db_passwdE = maybe [] (\x -> ["-pE", x]) (lookupFromMeta "db-pass-edit" meta)
  let db_prefix = maybe [] (\x -> ["-D", x]) (lookupFromMeta "db-prefix" meta)
  let init_sql = maybe [] (\x -> ["-i", x]) (lookupFromMeta "db-init-sql" meta)
  let inti_file = maybe [] (\x -> ["-I", x]) (lookupFromMeta "db-init-file" meta)
  let answer_sql = maybe "" id $ lookupFromMeta "answer-sql" meta
  withTemp "submit.sql" src $ \submittedFilePath -> do
    chmod readable submittedFilePath
    classify <$> unsafeExec evaluator
      (db_host ++ db_port ++ db_userS ++ db_passwdS ++ db_userE ++ db_passwdE ++ db_prefix
       ++ init_sql ++ inti_file ++ ["-a", answer_sql, "-S", submittedFilePath]) ""


classify :: (ExitCode, Text, Text) -> Result
classify (_, stdout, stderr)
  | match "Accepted" stderr            = accepted stdout
  | match "Wrong Answer" stderr        = wrongAnswer stdout
  | match "Runtime Error" stderr       = runtimeError stdout
  | match "Compile Error" stderr       = compileError stdout
  | match "Time Limit" stderr          = timeLimitExceeded stdout
  | match "Memory Limit" stderr        = memoryLimitExceeded stdout
  | otherwise                          = miscError (stderr <> stdout)
