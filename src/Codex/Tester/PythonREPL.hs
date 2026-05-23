{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
--
-- | Test Python programs using the REPL
--
module Codex.Tester.PythonREPL (
  pythonREPL
  ) where

import           Codex.Tester
import qualified Data.Text as T
import           Data.Text (Text)
import           Control.Concurrent.Async (concurrently)

import qualified Text.Pandoc.Builder as P

pythonREPL :: Tester Result
pythonREPL = tester "repl" $ checkForbidden $ do
  Code lang code <- testCode
  guard (lang == "python")
  cmdline <- parseArgs =<< configured "language.python.interpreter"
  assert (pure $ cmdline /= [])  "python interpreter not defined"
  profile <- configured "language.python.firejail"
  answer <- metadataWithDefault "answer" ""
  assert (pure $ answer /= "") "missing answer in metadata"
  -- public and private tests
  pub_tests <- parseTests <$> metadataWithDefault "public-tests" ""
  priv_tests <-parseTests <$> metadataWithDefault "private-tests" ""
  check <- checkCompile profile cmdline code
  case check of
    Just err -> return (compileError $ P.codeBlock err)
    Nothing -> do
      rs1 <- zipWithM (runTest profile cmdline answer code)
                           [1..] pub_tests
      rs2 <- zipWithM (runTest profile cmdline answer code)
                           [1+length pub_tests..] priv_tests
      return (label "Public tests" (mconcat rs1) <>
              label "Private tests" (private (mconcat rs2)) <>
              label "Summary" (summary (rs1 ++ rs2)))

summary :: [Result] -> Result
summary rs
  = let numTests = length rs 
        numPassed= length (filter isAccepted rs)
    in accepted $
       P.plain ("Total: " <> string (show numTests) <> " tests, " <>
                string (show numPassed) <> " passed.") 



runTest :: MonadIO m =>
           FilePath -> [String] -> Text -> Text -> Int -> Text
        -> m Result
runTest profile cmdline answer code number stdin = do
  let test =  P.text ("Test " <> T.pack (show number)) <> P.space
  (expected,
   obtained) <- liftIO $ concurrently
                             (runPython profile cmdline answer stdin)
                             (runPython profile cmdline code stdin)
  return $
    case (expected, obtained) of
      (Right out1, Right out2) ->
        if out1 == out2 then
          accepted $
          P.para (P.strong (test <> "passed")) <>
          P.codeBlock (showTest stdin) <>
          P.codeBlock out1
        else
          wrongAnswer $
          P.para (P.strong (test <> "failed") ) <> 
          P.codeBlock (showTest stdin) <>
          P.plain "Expected" <>
          P.codeBlock out1 <>
          P.plain "Obtained" <>
          P.codeBlock out2
      (Right _,  Left stderr) ->
        let handler
              | match "TimeLimitExceeded" stderr   = timeLimitExceeded
              | match "MemoryError" stderr         = memoryLimitExceeded
              | otherwise                          = runtimeError
        in handler $
           P.para (P.strong (test <> "failed")) <>
           P.codeBlock (showTest stdin) <>
           P.plain "Output" <>
           P.codeBlock stderr

      (Left stderr, _) ->
        miscError $
        P.para (P.strong (test <> "runtime error in solution")) <>
        P.codeBlock (showTest stdin) <>
        P.plain "Error" <>
        P.codeBlock stderr
     
checkCompile :: MonadIO m => FilePath -> [String] -> Text -> m (Maybe Text)
checkCompile profile cmdline code = do
      r <- runPython profile cmdline code ""
      return $ case r of
        Left err -> Just err
        Right _ -> Nothing
    

runPython :: MonadIO m =>
             FilePath -> [String] -> Text -> Text -> m (Either Text Text)
runPython _ [] _ _ 
  = error "runPython: empty command line"
runPython profile (python:args) code stdin =
  withTemp "submit.py" (code <> "\n\n" <> stdin) $
  \file -> do
    ProcessRun{..} <- sandboxExec profile python (args++[file]) ""
    return $ case procExitCode of
                 ExitFailure _ -> Left procStderr
                 ExitSuccess ->              
                   if T.length procStdout > defaultMaxOutput then
                     Left "*** Output limit exceeded ***"
                   else
                     if match "Error:" procStderr then
                       Left procStderr
                     else
                       Right procStdout


-- default maximum characters of output
defaultMaxOutput :: Int
defaultMaxOutput = 1000

string :: String -> P.Inlines
string = P.text . T.pack

-- parse tests
-- parseTests :: Text -> [Text]
-- parseTests 
--  = filter (not . T.null) . map T.strip . T.splitOn ">>>"
parseTests :: Text -> [Text]
parseTests = parsePrompt ">>>"

showTest :: Text -> Text
showTest = unparsePrompt ">>> "

unparsePrompt :: Text -> Text -> Text
unparsePrompt prompt text
  = T.unlines $
    case T.lines text of
      (first:rest) -> (prompt<>first) : map (padding<>) rest
      [] -> []
  where padding = T.replicate (T.length prompt) " "

parsePrompt :: Text -> Text -> [Text]
parsePrompt prompt text
  = case T.splitOn prompt text of
      (_:rest) -> map unindent rest
      _ -> []
  where
    unindent txt
      = T.unlines $
        case T.lines txt of
          [] -> [""]
          (first:rest) ->
            let
              first' = T.dropWhile (==' ') first
              len = T.length prompt + T.length first - T.length first'
            in
              first' : map (dropSpaces len) rest

-- | drop at most len spaces from the start of a text
dropSpaces :: Int -> Text -> Text
dropSpaces len txt = T.drop (min len len') txt
  where len' = T.length (T.takeWhile (==' ') txt)

