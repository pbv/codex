{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFunctor #-}
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

-- a Prolog query with some output
data Query a 
  = Success a       -- ^ query succeeded 
  | Failure a       -- ^ query failed
  | Error Text      -- ^ runtime error (with stderr)
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
prologREPL = tester "repl" $ language "prolog" $ checkForbidden $ do
  Code _ code <- testCode
  ---
  limits <- configLimits "language.prolog.limits"
  cmdline <- parseArgs =<< configured "language.prolog.interpreter"
  assert (pure $ cmdline /= [])  "prolog interpreter not defined"
  answer <- metadataWithDefault "answer" ""
  assert (pure $ answer /= "") "missing answer in metadata"
  -- public and private tests
  pub_tests <- parseTests <$> metadataWithDefault "public-tests" ""
  priv_tests <- parseTests <$> metadataWithDefault "private-tests" ""
  ordering <- metadataWithDefault "ignore-order" False
  outputLimit <- metadataWithDefault "output-limit" maxOutput
  let normalize = (if ordering then sortLines else id) .
                  cutOutput outputLimit
  rs1 <- zipWithM (runTest limits cmdline normalize answer code)
                           [1..] pub_tests
  rs2 <- zipWithM (runTest limits cmdline normalize answer code)
                           [length pub_tests+1..] priv_tests
  return (label "Public tests" (mconcat rs1) <>
          label "Private tests" (private (mconcat rs2)) <>
          label "Summary" (summary (rs1++rs2)))

summary :: [Result] -> Result
summary rs
  = let numTests = length rs 
        numPassed= length (filter isAccepted rs)
    in accepted $
       P.plain ("Total: " <> string (show numTests) <> " tests, " <>
                string (show numPassed) <> " passed.") 


         
runTest :: MonadIO m => Limits -> [String] -> (Text -> Text)
        -> Text -> Text -> Int -> Text
        -> m Result
runTest _     []      _     _         _      _    _
  = error "no prolog command; should not happen!"
runTest limits (prolog:args) normalize answer code number query = do
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
      (Success out1, Failure out2) ->
        wrongAnswer $
        P.para (P.strong (test <> "failed")) <>
        P.codeBlock query <>
        P.para "Query failed, expecting success"
      (Failure out1, Success out2) ->
        wrongAnswer $
        P.para (P.strong (test <> "failed")) <>
        P.codeBlock query <>
        P.para "Query succeeded, expecting failure" 
      (_, Error msg) ->
        let handler
              | match "Syntax error" msg          = compileError
              | match "Memory Limit Exceeded" msg = memoryLimitExceeded
              | match "Time Limit Exceeded" msg   = timeLimitExceeded
              | otherwise = runtimeError
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
    runQuery :: MonadIO m => Text -> Text -> m (Query Text)
    runQuery code query  =
      withTemp "code.pl" code $
      \file -> do
        chmod readable file
        (exitCode, stdout, stderr) <-
          safeExec limits prolog Nothing (args ++ ["-q", "-l", file, "-g", T.unpack query]) ""
        return $
          if match "ERROR:" stderr ||
             match "Time Limit Exceeded" stderr ||
             match "Memory Limit Exceeded" stderr
          then
            Error stderr
          else
            case exitCode of
              ExitSuccess -> Success stdout
              ExitFailure _ -> Failure stdout

-- default maximum characters of output
maxOutput :: Int
maxOutput = 1000

-- parse tests
parseTests :: Text -> [Text]
parseTests 
  = filter (not . T.null) . map T.strip . T.splitOn ">>>" 

showTest :: Text -> Text
showTest = (">>> " <>)

string :: String -> P.Inlines
string = P.text . T.pack

