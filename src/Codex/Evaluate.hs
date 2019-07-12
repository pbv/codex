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
import           Control.Monad.State
import           Control.Concurrent (ThreadId) 
import           Control.Exception  (SomeException, catch)
import           System.FilePath
import           Text.Printf

import           Snap.Snaplet
import qualified Snap.Snaplet.SqliteSimple as S

import           Data.Configurator.Types(Config)
import qualified Data.Configurator as Conf

import           Codex.Types
import           Codex.Application
import           Codex.Tasks
import           Codex.Utils
import           Codex.Page
import           Codex.Submission
import           Codex.Tester
import           Codex.Policy


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
  evaluateWith (forkTask semph) sub

-- | re-evaluate a list of submissions;
-- fork tasks under a (new) quantity semaphore for throttling concurrency;
-- manage tasks a queue to allow canceling
evaluateMany :: [Submission] -> Codex ()
evaluateMany subs = do
  conf <- getSnapletUserConfig
  maxtasks <- liftIO $ Conf.require conf "system.max_concurrent"
  semph <- newTaskSemph maxtasks
  queue <- gets _queue
  sequence_ [evaluateWith (addTask queue . forkTask semph) sub
            | sub <- subs]

-- | cancel all pending evaluations
cancelPending :: Codex ()
cancelPending = cancelTasks =<< gets _queue 


-- | evaluate a single submission with a given scheduling strategy
-- 
evaluateWith :: (IO () -> Codex r) -> Submission -> Codex r
evaluateWith schedule Submission{..} = do
  sqlite <- S.getSqliteState
  conf <- getSnapletUserConfig
  env <- getTimeEnv
  tester <- gets _tester
  filepath <- getDocumentRoot >>= return . (</> submitPath)
  schedule $ do
    updateSubmission sqlite submitId evaluating
    page <- readMarkdownFile filepath
    let constr = pageValid page
    (testWrapper tester conf filepath page Submission{..} >>=
     return . checkTiming env constr submitTime >>=
     checkAttempts sqlite constr Submission{..}  >>=
     updateSubmission sqlite submitId)


-- | append a policy check to a result
update :: Result -> Check -> Result
update r c = r { resultCheck = resultCheck r <> c }

            
checkTiming :: TimeEnv -> Policy TimeExpr -> UTCTime -> Result -> Result
checkTiming env constr time result 
  = update result $ case evalPolicy env constr of
    Left err -> Invalid err
    Right constr'
      | early time constr' -> Invalid "Early submission. "
      | late  time constr' -> Invalid "Late submission. "
      | otherwise -> Valid
      

-- | check maximum number of attempts
checkAttempts :: S.Sqlite
              -> Policy time
              -> Submission
              -> Result
              -> IO Result
checkAttempts sqlite constr submiss result
  = update result <$>
    case maxAttempts constr of
      Nothing ->
        return Valid
      Just max -> do
        count <- countEarlierSubmissions sqlite submiss
        return $ if count < max then Valid
                 else let msg = printf "Over %d submissions. " max
                      in Invalid $ T.pack msg


-- | wrapper to run a tester and catch any exception
testWrapper :: Tester Result
            -> Config
            -> FilePath
            -> Page
            -> Submission
            -> IO Result
testWrapper tester conf filepath page submiss
  = (fromMaybe invalidTester <$> runTester tester conf filepath page submiss)
    `catch`
    (\(e::SomeException) -> return (miscError $ T.pack $ show e))
  

invalidTester :: Result
invalidTester = miscError "no acceptable tester configured"


