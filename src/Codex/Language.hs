{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Codex.Language(
  evaluateWith, 
  evaluate   -- ^ evaluate a single submission
  ) where

import qualified Data.Text                                   as T
import           Data.Monoid
import           Data.Maybe
import           Data.Configurator.Types
import           Data.Time.LocalTime
import           Control.Applicative
import           Control.Monad.State
import           Control.Concurrent(ThreadId, forkIO)
import           Control.Exception  (SomeException)
import           Control.Exception.Lifted  (catch)

import           Snap.Snaplet
import qualified Snap.Snaplet.SqliteSimple                   as S

import           Codex.Application
import           Codex.Config
import           Codex.Types
import           Codex.Utils
import           Codex.Page
import           Codex.Submission
import           Codex.Tester
import           Codex.Interval
import           Codex.Markdown

import           Codex.Language.Python 
import           Codex.Language.Haskell
import           Codex.Language.C 


-- | all language testers
allTesters :: Config -> Page -> Code -> Tester Result
allTesters conf page code =
  pythonTester conf page code <|>
  haskellTester conf page code <|>
  clangTester conf page code

-- | default evaluator
evaluate :: Submission -> Codex ThreadId
evaluate = evaluateWith allTesters 


-- evaluate a submission (1st time or re-evaluation) with a specific tester;
-- runs code tester in separate thread
-- uses a semaphore for "throttling" evaluations 
evaluateWith :: (Config -> Page -> Code -> Tester Result)
             -> Submission
             -> Codex ThreadId
evaluateWith testf sub = do
  sqlite <- S.getSqliteState
  evs <- getEvents
  conf <- getSnapletUserConfig
  sem <- gets evalQS
  liftIO $ forkIO $ withQSem sem $ do
    tz <- getCurrentTimeZone
    page <- readPage publicPath (submitPath sub)
    let sid = submitID sub        -- ^ submission number
    let optT = rankTime (submitTime sub) <$> evalI tz evs (submitInterval page)
    case optT of
      Nothing ->
        updateSubmission sqlite sid (wrongInterval page) Valid
      Just t -> do
        putStrLn $ "start evaluation of submission " ++ show (fromSID sid)
        let code = submitCode sub     -- ^ program code
        mayResult <- runTester (testf conf page code)
                     `catch`
                     (\(e::SomeException) ->
                         return (Just $ miscError $ T.pack $ show e))
        let result = fromMaybe (missingTester code) mayResult
        updateSubmission sqlite sid result t
        putStrLn $ "end evaluation of submission " ++ show (fromSID sid)


wrongInterval :: Page -> Result
wrongInterval page =
  let valid = fromMaybe "" $ lookupFromMeta "valid" (pageMeta page)
  in miscError $ "Invalid submission interval \"" <> valid <> "\""
  

missingTester :: Code -> Result
missingTester code = miscError $
  "No tester defined for language \"" <> fromLanguage (codeLang code) <> "\""

