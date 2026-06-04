{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RecordWildCards #-}
--
-- | Test Prolog programs
--
module Codex.Tester.PrologREPL (
  prologREPL
  ) where

import           Codex.Tester
import qualified Data.Text as T
import           Data.Text (Text)
import           Data.List (sort)
import           Control.Concurrent.Async (concurrently)

import qualified Text.Pandoc.Builder as P

-- the result of a Prolog query 
data Answer a 
  = Success a       -- ^ a query that succeeded 
  | Failure a       -- ^ a query that failed
  | Error Text      -- ^ runtime error 
  deriving (Eq, Show, Functor)

-- normalize the query output
sortLines :: Text -> Text
sortLines = T.unlines . sort . T.lines

cutOutput :: Int -> Text -> Text
cutOutput n txt
  | T.length txt <= n = txt
  | otherwise = T.take n txt <> "\n*** Output limit exceeded ***\n"

--
-- | Tester for Prolog queries
--
prologREPL :: Tester Result
prologREPL = tester "repl" $ checkForbidden $ do
  Code lang code <- testCode
  guard (lang == "prolog")
  ---
  cmdline <- parseArgs =<< configured "language.prolog.interpreter"
  assert (pure $ cmdline /= [])  "prolog interpreter not defined"
  answer <- metadataWithDefault "answer" ""
  assert (pure $ answer /= "") "missing answer in metadata"
  -- public and private tests
  pubTests <- parseTests <$> metadataWithDefault "public-tests" ""
  privTests <- parseTests <$> metadataWithDefault "private-tests" ""
  ordering <- metadataWithDefault "ignore-order" False
  outputLimit <- metadataWithDefault "output-limit" maxOutput
  let normalize = (if ordering then sortLines else id) . cutOutput outputLimit
  profile <- configured "language.prolog.firejail"
  dir <- takeDirectory <$> testFilePath  
  files <- globPatterns dir =<< metadataWithDefault "extra-files" []  
  withTempDir "codex" $ \tmpdir -> do
    copyFiles files tmpdir
    let runner = runTest profile cmdline normalize tmpdir answer code
    rs1 <- zipWithM runner [1..] pubTests
    rs2 <- zipWithM runner [length pubTests+1..] privTests
    return (label "Public tests" (mconcat rs1)
            <>
            label "Private tests" (private (mconcat rs2))
            <>
            label "Summary" (summary (rs1++rs2)))

summary :: [Result] -> Result
summary rs
  = let numTests = length rs 
        numPassed= length (filter isAccepted rs)
    in accepted $
       P.plain ("Total: " <> string (show numTests) <> " tests, " <>
                string (show numPassed) <> " passed.") 


         
runTest ::
  MonadIO m =>
  FilePath -> [String] -> (Text -> Text) -> FilePath 
  -> Text -> Text -> Int -> Text -> m Result
runTest _     []      _     _         _      _    _ _
  = error "internal error: empty prolog command"
runTest profile (prolog:args) normalize dir answer code number query = do
  let test =  P.text ("Test " <> T.pack (show number)) <> P.space
  (expected,
   obtained) <- liftIO $ concurrently 
                          (fmap normalize <$> runQuery answer query)
                          (fmap normalize <$> runQuery code query)
  return $
    case (expected, obtained) of
      (Success out1, Success out2) ->
        if out1 == out2 then
          accepted $
          P.para (P.strong (test <> "passed")) <>
          P.codeBlock query
        else
          wrongAnswer $
          P.para (P.strong (test <> "failed")) <>
          P.codeBlock query <>
          P.plain "Expected" <>
          P.codeBlock out1 <>
          P.plain "Obtained"<>
          P.codeBlock out2 
      (Failure out1, Failure out2) ->
        if out1 == out2 then
          accepted $
          P.para (P.strong (test <> "passed")) <>
          P.codeBlock query
        else
          wrongAnswer $ 
          P.para (P.strong (test <> "failed")) <>
          P.codeBlock query <>
          P.plain "Expected" <>
          P.codeBlock out1 <>
          P.plain "Obtained" <>
          P.codeBlock out2
      (Success _, Failure _) ->
        wrongAnswer $
        P.para (P.strong (test <> "failed")) <>
        P.codeBlock query <>
        P.para "Query failed, expecting success"
      (Failure _, Success _) ->
        wrongAnswer $
        P.para (P.strong (test <> "failed")) <>
        P.codeBlock query <>
        P.para "Query succeeded, expecting failure" 
      (_, Error msg) ->
        let handler
              | match "Syntax error" msg          = compileError
              | match "Stack limit" msg           = memoryLimitExceeded
              | match "TimeLimitExceeded" msg     = timeLimitExceeded
              | otherwise                         = runtimeError
        in handler $
           P.para (P.strong (test <> "failed")) <>
           P.codeBlock query <>
           P.codeBlock msg
      (Error msg, _) ->
        miscError $
        P.para (P.strong (test <> "runtime error in solution"))
        <> P.codeBlock query
        <> P.codeBlock msg

  where
    runQuery :: MonadIO m => Text -> Text -> m (Answer Text)
    runQuery code query  =
      withTempText "code.pl" code $
      \file -> do
        ProcessRun{..} <-
          sandboxExec profile prolog (Just dir) (args ++ ["-q", "-l", file, "-g", T.unpack query]) ""
        return $
          if match "TimeLimitExceeded" procStderr ||
             match "Stack Limit" procStderr ||
             match "ERROR:" procStderr 
          then
            Error procStderr
          else
            case procExitCode of
              ExitSuccess -> Success procStdout
              ExitFailure _ -> Failure procStdout

-- default maximum characters of output
maxOutput :: Int
maxOutput = 1000

-- parse tests
parseTests :: Text -> [Text]
parseTests 
  = filter (not . T.null) . map T.strip . T.splitOn ">>>" 

-- showTest :: Text -> Text
-- showTest = (">>> " <>)

string :: String -> P.Inlines
string = P.text . T.pack

