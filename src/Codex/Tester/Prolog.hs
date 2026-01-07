{-# LANGUAGE OverloadedStrings #-}
--
-- | Test Prolog programs
--
module Codex.Tester.Prolog (
  prologTester
  ) where

import           Codex.Tester
import qualified Data.Text as T
import           Data.Text (Text)
import           Data.List (sort)

import qualified Text.Pandoc.Builder as P

-- result of a prolog query
data QueryResult
  = Success Text    -- ^ query succeeded and stdout 
  | Failure Text    -- ^ query failed and stdout 
  | Error Text      -- ^ runtime error and stderr
  deriving (Eq, Show)

-- normalize the query output
sortLines :: Text -> Text
sortLines = T.unlines . sort . T.lines

cutOutput :: Int -> Text -> Text
cutOutput n txt
  | T.length txt <= n = txt
  | otherwise = T.take n txt <> "\n*** Output limit exceeded ***\n"



prologTester :: Tester Result
prologTester = queryTester

--
-- | Tester for queries
--
queryTester :: Tester Result
queryTester = tester "queries" $ do
  Code lang code <- testCode
  guard (lang == "prolog")
  ---
  limits <- configLimits "language.prolog.limits"
  prolog <- parseArgs =<< configured "language.prolog.interpreter"
  assert (pure $ prolog /= [])  "prolog interpreter not defined"
  answer <- metadataWithDefault "answer" ""
  assert (pure $ answer /= "") "missing answer in metadata"
  -- public and private tests
  pub_tests <- metadataWithDefault "public-tests" []
  priv_tests <- metadataWithDefault "private-tests" []
  ordering <- metadataWithDefault "ignore-order" False
  outputLimit <- metadataWithDefault "output-limit" maxOutput
  let normalize = (if ordering then sortLines else id) .
                  cutOutput outputLimit
  rs1 <- liftIO $
         mapM (runTest limits prolog normalize answer code) pub_tests
  rs2 <- liftIO $
         mapM (runTest limits prolog normalize answer code) priv_tests
  return (tagWith Public (mconcat rs1) <> tagWith Private (mconcat rs2))

runTest :: Limits -> [String] -> (Text -> Text)
        -> Text -> Text -> Text
        -> IO Result
runTest _     []            _         _      _    _
  = error "no prolog command; should not happen!"
runTest limits (prolog:args) normalize answer code query = do
  expectOut <- runQuery answer query normalize
  obtainOut <- runQuery code query normalize
  return $
    case (expectOut, obtainOut) of
      (Success out1, Success out2) ->
        if out1 == out2 then
          accepted $
          P.para (P.text "Test passed: " <> P.code query)
        else
          wrongAnswer $
          P.para (P.text "Test failed: " <> P.code query)
          <> P.para "Output mismatch"
          <>
           P.simpleTable [ P.plain "Expected",
                           P.plain "Obtained" ]
          [ [ P.codeBlock out1
            , P.codeBlock out2 ]
          ]
      (Failure out1, Failure out2) ->
        if out1 == out2 then
          accepted $
          P.para (P.text "Test passed: " <> P.code query)
        else
          wrongAnswer $ 
          P.para (P.text "Test failed: " <> P.code query)
          <> P.para "Output mismatch"
          <>
          P.simpleTable [P.plain "Expected",
                         P.plain "Obtained" ]
          [ [ P.codeBlock out1
            , P.codeBlock out2 ]
          ]
      (Success out1, Failure out2) ->
        wrongAnswer $
        P.para (P.text "Test failed: " <> P.code query) <>
        P.para "Query failed, expecting success"
      (Failure out1, Success out2) ->
        wrongAnswer $
        P.para (P.text "Test failed: " <> P.code query) <>
        P.para "Query succeeded, expecting failure" 
      (_, Error msg) ->
        let result
              | match "Memory Limit Exceeded" msg = memoryLimitExceeded
              | match "Time Limit Exceeded" msg = timeLimitExceeded
              | otherwise = runtimeError
        in result $
           P.para (P.text "Test failed: " <> P.code query)
           <>
           P.codeBlock msg

      (Error msg, _) ->
        miscError $
        P.para (P.text "INVALID test case: " <> P.code query)
        <>
        P.codeBlock msg

  where
    runQuery :: Text -> Text -> (Text -> Text) -> IO QueryResult
    runQuery code query normalize =
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
              ExitSuccess -> Success (normalize stdout)
              ExitFailure _ -> Failure (normalize stdout)

-- default maximum characters of output
maxOutput :: Int
maxOutput = 10000

