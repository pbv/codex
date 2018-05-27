{-# LANGUAGE OverloadedStrings #-}
module Codex.Tester.SqlSelect (
  sqlSelectTester
  ) where

import           Codex.Tester
import           Data.Text(Text)


sqlSelectTester :: PageInfo -> Code -> Test Result
sqlSelectTester (PageInfo path meta) (Code lang src) = do
  guard (lang == "sql")
  guard (tester meta == Just "select")
  ---
  evaluator <- configured "language.sql.select.evaluator"
  let answerFilePath = getAnswerFilePath (PageInfo path meta)
  assert (fileExists answerFilePath)
      ("doctest file not found: " <> show answerFilePath)
  chmod readable answerFilePath
  withTemp "submit.sql" src $ \submittedFilePath -> do
    chmod readable submittedFilePath
    classify <$>
      unsafeExec evaluator [answerFilePath, submittedFilePath] ""

classify :: (ExitCode, Text, Text) -> Result
classify (code, stdout, stderr)
  | code == ExitSuccess                 = accepted ""
  | match "Time Limit Exceed" stderr    = timeLimitExceeded stdout
  | match "Memory Limit Exceed" stderr  = memoryLimitExceeded stdout
  | match "Runtime Error" stderr        = runtimeError stdout
  | match "Syntax Error" stderr         = compileError stdout
  | match "Wrong Answer" stderr         = wrongAnswer stdout
  | otherwise                           = miscError (stdout <> stderr)

-- | guess the test path from page metadata or filename
getAnswerFilePath :: PageInfo -> FilePath
getAnswerFilePath (PageInfo path meta)
  = maybe
       (replaceExtension path ".sql")
       (takeDirectory path </>)
       (lookupFromMeta "answer-file" meta)
