{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PatternGuards #-}

module Codex.Evaluate(
  newSubmission,
  evaluate,
  evaluateMany,
  cancelPending,
  withPolicySplices,
  ) where

import qualified Data.Text as T
import           Data.Maybe
import           Data.List (intersperse)
import           Data.Time.Clock
import           Data.Time.LocalTime
import           Control.Monad.Writer
import           Control.Monad.State
import           Control.Concurrent (ThreadId) 
import           Control.Exception  (SomeException, catch)
import           System.FilePath

import           Snap.Snaplet
import           Snap.Snaplet.Heist
import qualified Snap.Snaplet.SqliteSimple as S
import           Heist.Splices     as I
import qualified Heist.Interpreted as I
import           Data.Map.Syntax


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


-- | a submission attempt by a user
data Access = Access { accessUser :: UserLogin
                     , accessPath :: FilePath
                     , accessTime :: UTCTime
                     }


-- | insert a new submission into the database
-- also schedule evaluation in separate thread
newSubmission :: UserLogin -> FilePath -> Code -> Codex SubmitId
newSubmission uid rqpath code = do
  now <- liftIO getCurrentTime
  sub <- insertSubmission uid rqpath now code evaluating (invalid "?")
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
evaluateWith async submission@Submission{..} = do
  conn <- S.getSqliteState
  conf <- getSnapletUserConfig
  env <- getTimeEnv
  tester <- gets _tester
  root <- getDocumentRoot
  let filepath = root </> submitPath
  async $ do    -- run the tester asynchronously
    runSqlite conn $ updateSubmission submitId evaluating (invalid "?")
    page <- readMarkdownFile filepath
    let attempt = Access { accessUser = submitUser
                         , accessPath = submitPath
                         , accessTime = submitTime
                         }
    check <- case evalPolicy env =<< pagePolicy page of
               Left err -> return (invalid err)
               Right policy ->
                 runSqlite conn $ checkPolicy attempt policy
    result <- testWrapper tester conf filepath page submission 
    runSqlite conn $ updateSubmission submitId result check


-- | wrapper to run a tester and catch any exceptions
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


withPolicySplices :: UserLogin -> FilePath -> Page -> Codex a -> Codex a
withPolicySplices uid path page action = do
  tz <- liftIO getCurrentTimeZone
  now <- liftIO getCurrentTime
  constrs <- getPolicy page
  let attempt = Access { accessUser = uid
                       , accessPath = path
                       , accessTime = now
                       }
  check <- checkPolicy attempt constrs
  earlier <- countPageSubmissions uid path
  let allow = pageAllowInvalid page 
  let
    splice :: Constr UTCTime -> ISplices
    splice (After start) = do
      "submit-after" ## I.textSplice (formatLocalTime tz start)
      "if-submit-after" ## I.ifElseISplice True
      "if-submit-early" ## I.ifElseISplice (now < start)
    splice (Before limit) = do
      let remain = max 0 (diffUTCTime limit now)
      "submit-before" ## I.textSplice (formatLocalTime tz limit)
      "submit-time-remain" ## I.textSplice (formatNominalDiffTime remain)
      "if-submit-before" ## I.ifElseISplice True
      "if-submit-late" ## I.ifElseISplice (now > limit)
    splice (MaxAttempts limit) = do
      let remain = max 0 (limit - earlier)
      "submissions-attempts" ## I.textSplice (T.pack $ show limit)
      "submissions-remain" ## I.textSplice (T.pack $ show remain)
      "if-submit-attempts" ## I.ifElseISplice True
  flip withSplices action $ do
    "if-valid" ## I.ifElseISplice (check == Valid)
    "if-allowed" ## I.ifElseISplice (allow || check == Valid)
    "invalid-msg" ## case check of
                       Valid -> return []
                       Invalid msg -> I.textSplice msg
    "if-submitted" ## I.ifElseISplice (earlier > 0)
    "if-submit-after" ## I.ifElseISplice False
    "if-submit-before" ## I.ifElseISplice False
    "if-submit-attempts" ## I.ifElseISplice False
    "if-submit-early" ## I.ifElseISplice False
    "if-submit-late" ## I.ifElseISplice False
    mapM_ splice constrs


      
-- | check the policy specified for a page
checkPolicy :: S.HasSqlite m
            => Access -> Policy UTCTime -> m Validity
checkPolicy  attempt constrs = do
  (_, msgs) <- runWriterT (mapM_ (checkConstr attempt) constrs)
  return $ if null msgs then Valid
           else Invalid (T.concat $ intersperse ";" msgs)

checkConstr :: S.HasSqlite m
            => Access -> Constr UTCTime -> WriterT [Text] m ()
checkConstr Access{..} (Before highTime)
  | accessTime < highTime = return ()
  | otherwise       = tell ["Late submission"]
checkConstr Access{..} (After highTime)
  | accessTime > highTime = return ()
  | otherwise       = tell ["Early submission"]
checkConstr Access{..} (MaxAttempts max) = do
  earlier <- lift $ countEarlierSubmissions accessUser accessPath accessTime
  if earlier < max then return ()
    else tell ["Too many submissions"]
