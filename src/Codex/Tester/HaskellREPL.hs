{-# LANGUAGE OverloadedStrings #-}
--
-- | Test Haskell programs using GHCi
--
module Codex.Tester.HaskellREPL (
  haskellREPL
  ) where

import           Codex.Tester
import qualified Data.Text as T
import           Data.Text (Text)
import           Control.Concurrent.Async (concurrently)

import qualified Text.Pandoc.Builder as P



haskellREPL :: Tester Result
haskellREPL = tester "repl" $ language "haskell" $ checkForbidden $ do
  Code _ code <- testCode
  limits <- configLimits "language.haskell.limits"
  -- verbosity <- metadataWithDefault "verbosity" 1
  cmdline <- parseArgs =<< configured "language.haskell.interpreter"
  assert (pure $ cmdline /= [])  "haskell interpreter not defined"
  answer <- metadataWithDefault "answer" ""
  assert (pure $ answer /= "") "missing answer in metadata"
  -- public and private tests
  pub_tests <- parseTests <$> metadataWithDefault "public-tests" ""
  priv_tests <-parseTests <$> metadataWithDefault "private-tests" ""
  check <- checkCompile limits cmdline code
  case check of
    Just err -> return (compileError $ P.codeBlock err)
    Nothing -> do
      rs1 <- zipWithM (runTest limits cmdline answer code)
                           [1..] pub_tests
      rs2 <- zipWithM (runTest limits cmdline answer code)
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
           Limits -> [String] -> Text -> Text -> Int -> Text
        -> m Result
runTest limits cmdline answer code number stdin = do
  let test =  P.text ("Test " <> T.pack (show number)) <> P.space
  (expected,
   obtained) <- liftIO $ concurrently
                             (runGhci limits cmdline answer stdin)
                             (runGhci limits cmdline code stdin)
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
      (Right _, Left stderr) ->
        let handler
              | match "error:" stderr                = compileError
              | match "Memory Limit Exceeded" stderr = memoryLimitExceeded
              | match "Time Limit Exceeded" stderr   = timeLimitExceeded
              | otherwise = runtimeError
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
        



checkCompile :: MonadIO m => Limits -> [String] -> Text -> m (Maybe Text)
checkCompile limits cmdline code = do
      r <- runGhci limits cmdline code ""
      return $ case r of
        Left err -> Just err
        Right _ -> Nothing
    

runGhci :: MonadIO m =>
           Limits -> [String] -> Text -> Text -> m (Either Text Text)
runGhci _      []          _    _
  = error "runChci: empty command line"
runGhci limits (ghci:args) code stdin =
  withTemp "submit.hs" code $
  \file -> do
    chmod readable file
    (exitCode, stdout, stderr) <-
      safeExec limits ghci Nothing (args ++ ["-v0", "-ignore-dot-ghci", file]) stdin
    return $ case exitCode of
                 ExitFailure _ -> Left stderr
                 ExitSuccess ->
                    if match "error:" stderr then
                      Left stderr
                    else
                      if T.length stdout > defaultMaxOutput then
                        Left "*** Output limit exceeded ***"
                      else
                        Right stdout


-- default maximum characters of output
defaultMaxOutput :: Int
defaultMaxOutput = 1000

string :: String -> P.Inlines
string = P.text . T.pack

-- parse tests
parseTests :: Text -> [Text]
parseTests 
  = filter (not . T.null) . map T.strip . T.splitOn ">>>" 

showTest :: Text -> Text
showTest = (">>> " <>)
