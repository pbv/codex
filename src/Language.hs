{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Language(
  module Language.Types,
  evaluate   -- ^ evaluate a single submission
  ) where

import qualified Data.Text                                   as T
import           Data.Monoid
import           Data.Configurator.Types
import           Data.Time.LocalTime
import           Control.Monad.State
import           Control.Concurrent(ThreadId, forkIO)
import           Control.Exception  (SomeException)
import           Control.Exception.Lifted  (catch)

import           Snap.Snaplet
import qualified Snap.Snaplet.SqliteSimple                   as S

import           Application
import           Config
import           Utils
import           Page
import           Submission
import           Tester
import           Interval

import           Language.Types  
import           Language.Python 
import           Language.Haskell
import           Language.C 


-- evaluate a submission (1st time or re-evaluation);
-- runs code tester in separate thread
evaluate :: Submission -> Codex (Maybe ThreadId)
evaluate sub = do
  let sid = submitID sub        -- ^ submission number
  let code = submitCode sub     -- ^ program code
  let time = submitTime sub     -- ^ time received
  page <- liftIO $ readPage publicPath (submitPath sub)
  sqlite <- S.getSqliteState
  -- evaluate submission timing 
  evs <- getEvents
  tz <- liftIO getCurrentTimeZone
  let optT = timing time <$> evalI tz evs (submitInterval page)
  case optT of
    Nothing -> do
      let res = miscError "Invalid submission interval"
      liftIO $ updateSubmission sqlite sid res Overdue
      return Nothing
    Just tv -> do
      conf <- getSnapletUserConfig
      sem <- gets evalQS
      -- fork a thread for evaluating submission
      tid <- liftIO $ forkIO $ withQSem sem $ do
        -- putStrLn $ "start evaluation of " ++ show sid
        updateSubmission sqlite sid evaluating tv
        result <- codeTester conf page code `catch`
                  (\ (e::SomeException) -> return (miscError $ T.pack $ show e))
        updateSubmission sqlite sid result tv
        -- putStrLn $ "end evaluation of " ++ show sid
      return (Just tid)



codeTester :: Config -> Page -> Code -> IO Result
codeTester conf page code
  = case codeLang code of
    Language "python" ->
        pythonTester conf page code
    Language "haskell" ->
        haskellTester conf page code
    Language "c" -> 
         clangTester conf page code
    _ -> return (received errMsg)
  where
    errMsg = "No tester defined for \"" <>fromLanguage (codeLang code) <> "\""

