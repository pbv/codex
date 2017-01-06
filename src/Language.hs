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
evaluate :: Submission -> Codex ThreadId
evaluate sub = do
  sqlite <- S.getSqliteState
  evs <- getEvents
  conf <- getSnapletUserConfig
  -- get semaphore for "throttling" evaluation and fork an evaluation thread
  sem <- gets evalQS
  liftIO $ forkIO $ withQSem sem $ do
    tz <- getCurrentTimeZone
    page <- readPage publicPath (submitPath sub)
    let sid = submitID sub        -- ^ submission number
    let optT = rankTime (submitTime sub) <$> evalI tz evs (submitInterval page)
    case optT of
      Nothing -> 
        updateSubmission sqlite sid (miscError "Invalid submission interval") Overdue
      Just t -> do
        putStrLn $ "start evaluation of " ++ show sid
        -- updateSubmission sqlite sid evaluating tv
        let code = submitCode sub     -- ^ program code
        result <- codeTester conf page code `catch`
                  (\ (e::SomeException) -> return (miscError $ T.pack $ show e))
        updateSubmission sqlite sid result t
        putStrLn $ "end evaluation of " ++ show sid


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

