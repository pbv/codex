{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
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
haskellREPL = tester "repl" $ checkForbidden $ do
  Code lang code <- testCode
  guard (lang == "haskell")
  cmdline <- parseArgs =<< configured "language.haskell.interpreter"
  assert (pure $ cmdline /= [])  "haskell interpreter not defined"
  profile <- configured "language.haskell.firejail"
  answer <- metadataWithDefault "answer" ""
  assert (pure $ answer /= "") "missing answer in metadata"
  -- public and private tests
  pub_tests <- parseTests <$> metadataWithDefault "public-tests" ""
  priv_tests <-parseTests <$> metadataWithDefault "private-tests" ""
  dir <- takeDirectory <$> testFilePath  
  files <- globPatterns dir =<< metadataWithDefault "extra-files" []
  withTempDir "codex" $ \tmpdir -> do
    copyFiles files tmpdir
    check <- checkCompile profile tmpdir cmdline code
    case check of
      Just err -> return (compileError $ P.codeBlock err)
      Nothing -> do
        let runner = runTest profile tmpdir cmdline answer code
        rs1 <- zipWithM runner [1..] pub_tests
        rs2 <- zipWithM runner [1+length pub_tests..] priv_tests
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
           FilePath -> FilePath -> [String] -> Text -> Text -> Int -> Text
        -> m Result
runTest profile tmpdir cmdline answer code number stdin = do
  let test = P.text ("Test " <> T.pack (show number)) <> P.space
  (expected,
   obtained) <- liftIO $ concurrently
                             (runGhci profile tmpdir cmdline answer stdin)
                             (runGhci profile tmpdir cmdline code stdin)
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
              | match "TimeLimitExceeded"  stderr = timeLimitExceeded
              | match "out of memory" stderr      = memoryLimitExceeded
              | match "error:" stderr             = compileError
              | otherwise                         = runtimeError
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
        



checkCompile :: MonadIO m => FilePath -> FilePath -> [String] -> Text -> m (Maybe Text)
checkCompile profile tmpdir cmdline code = do
      r <- runGhci profile tmpdir cmdline code ""
      return $ case r of
        Left err -> Just err
        Right _ -> Nothing
    

runGhci ::
  MonadIO m =>
  FilePath -> FilePath -> [String] -> Text -> Text
  -> m (Either Text Text)
runGhci _  _    []          _    _
  = error "runChci: empty command line"
runGhci profile tmpdir (ghci:args) code stdin =
  withTempText "submit.hs" code $
  \file -> do
    ProcessRun{..} <-
      sandboxExec profile ghci (Just tmpdir) (args ++ ["-v0", "-ignore-dot-ghci", file]) stdin
    return $ case procExitCode of
                 ExitFailure _ -> Left procStderr
                 ExitSuccess ->
                    if match "error:" procStderr then
                      Left procStderr
                    else
                      if T.length procStdout > defaultMaxOutput then
                        Left "*** Output limit exceeded ***"
                      else
                        Right procStdout


-- default maximum characters of output
defaultMaxOutput :: Int
defaultMaxOutput = 1000

string :: String -> P.Inlines
string = P.text . T.pack

