{-# LANGUAGE OverloadedStrings #-}
module Codex.Tester.SqlSelect (
  sqlSelectTester
  ) where

import           Codex.Tester
import           Data.Text(Text)
import qualified Data.Text as T


sqlSelectTester :: PageInfo -> Code -> Test Result
sqlSelectTester (PageInfo path meta) (Code lang src) = do
  guard (lang == "sql")
  guard (tester meta == Just "select")
  ---
  evaluator <- configured "language.sql.select.evaluator"
  limits  <- getLimits "language.sql.select.limits"
  let answerFilePath = getAnswerFilePath (PageInfo path meta)
  assert (fileExists answerFilePath)
      ("doctest file not found: " <> show answerFilePath)
  chmod readable answerFilePath
  withTemp "submit.sql" src $ \submittedFilePath -> do
    chmod readable submittedFilePath
    classify <$>
      safeExec limits evaluator [answerFilePath, submittedFilePath] ""

classify :: (ExitCode, Text, Text) -> Result
classify (_, stdout, stderr)
  | T.null stdout && match "OK" stderr = accepted stderr
  | match "Time Limit" stderr          = timeLimitExceeded stderr
  | match "Memory Limit" stderr        = memoryLimitExceeded stderr
  | match "Exception Raised" stdout    = runtimeError stdout
  | match "SyntaxError" stderr         = compileError stderr
  | match "Failed" stdout              = wrongAnswer stdout
  | otherwise                          = miscError (stdout <> stderr)

-- | guess the test path from page metadata or filename
getAnswerFilePath :: PageInfo -> FilePath
getAnswerFilePath (PageInfo path meta)
  = maybe
       (replaceExtension path ".sql")
       (takeDirectory path </>)
       (lookupFromMeta "answer-file" meta)
