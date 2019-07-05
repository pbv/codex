{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Codex.Evaluate(
  newSubmission,
  evaluate,
  evaluateMany,
  cancelPending
  ) where

import qualified Data.Text as T
import           Data.Maybe
import           Data.Time.Clock
-- import           Data.Time.LocalTime
import           Control.Monad.State
import           Control.Concurrent(ThreadId) 
import           Control.Exception  (SomeException, catch)
import           System.FilePath

import           Snap.Snaplet
import qualified Snap.Snaplet.SqliteSimple as S

import           Data.Configurator.Types(Config)

import           Codex.Types
import           Codex.Application
import           Codex.Tasks
import           Codex.Utils
import           Codex.Page
import           Codex.Submission
import           Codex.Tester
import           Codex.Time


-- | insert a new submission into the database
-- also schedule evaluation in separate thread
newSubmission :: UserLogin -> FilePath -> Code -> Codex SubmitId
newSubmission uid rqpath code = do
  now <- liftIO getCurrentTime
  sub <- insertSubmission uid rqpath now code evaluating
  evaluate sub
  return (submitId sub)


-- | evaluate a submission
-- fork an IO thread under a quantity semaphore for limiting concurrency 
evaluate :: Submission -> Codex ThreadId
evaluate sub = do
  semph <- gets _semph
  evaluateWith (forkQSem semph) sub

-- | evaluate a list of submissions;
-- fork IO threads under a quantity semaphore for limiting concurrency;
-- record thread ids in a queue to allow canceling
evaluateMany :: [Submission] -> Codex ()
evaluateMany subs = do
  semph <- gets _semph
  queue <- gets _queue
  sequence_ [evaluateWith (addQueue queue . forkQSem semph) sub
            | sub <- subs]

-- | cancel all pending evaluations
cancelPending :: Codex ()
cancelPending = cancelQueue =<< gets _queue 


-- | evaluate a single submission with a given scheduling strategy
-- 
evaluateWith :: (IO () -> Codex r) -> Submission -> Codex r
evaluateWith schedule Submission{..} = do
  sqlite <- S.getSqliteState
  conf <- getSnapletUserConfig
  env <- getTimeEnv
  tester <- gets _tester
  filepath <- fmap (</> submitPath) getDocumentRoot
  schedule $ do
    updateSubmission sqlite submitId evaluating
    page <- readMarkdownFile filepath
    result <- testWrapper tester conf page filepath submitUser submitCode
    let result' = checkTiming env (pageValid page) submitTime result
    updateSubmission sqlite submitId result'


            
checkTiming :: TimeEnv -> Constraint Time -> UTCTime -> Result -> Result
checkTiming env constr time result 
  = check result $ case evalConstraint env constr of
    Left err -> Invalid err
    Right constr'
      | early time constr' -> Invalid "Early submission."
      | late  time constr' -> Invalid "Late submission."
      | otherwise -> Valid

check :: Result -> Check -> Result
check r c = r { resultCheck = resultCheck r <> c }
      

{-
checkAttempts :: MonadIO m
              => Connection
              -> UserLogin
              -> FilePath
              -> Constraint t
              -> Result
              -> m Result
checkAttempt sqlite uid path constr result
  = case maxAttempts constr of
      Fin max -> do 
  | attempts > maxAttempts constr = Invalid "Too many attempts"
  | otherwise                     = Valid
-}


-- | wrapper to run a tester and catch any exception
testWrapper :: Tester Result
            -> Config
            -> Page
            -> FilePath
            -> UserLogin
            -> Code
            -> IO Result
testWrapper tester conf page path user code
  = (fromMaybe invalidTester <$> runTester conf page path code user tester)
    `catch`
    (\(e::SomeException) -> return (miscError $ T.pack $ show e))
  

invalidTester :: Result
invalidTester = miscError "no acceptable tester configured"


