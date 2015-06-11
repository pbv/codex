{-# LANGUAGE OverloadedStrings,  RecordWildCards #-}
module SubmitSummary
    ( ProblemSummary(..)
    , getProblemSummary
    --, getSubmitSummary
    -- , submitAvailable
    ) where


import            Types
import            Problem
import            Submission
import            Application
import           Control.Monad.State
import           Control.Applicative ((<$>))

import qualified  Interval as Interval
import qualified  Data.Set as Set
import qualified  Data.Map as Map
import            Data.Time.Clock



-- | submission summary for a problem
data ProblemSummary = ProblemSummary {
      summaryProb :: Problem, -- the problem
      summaryAttempts :: Int,  -- total number of submissions
      summaryAccepted :: Int  -- number of accepted submitions
    }


instance Tagged ProblemSummary where
  taglist ProblemSummary{..} = dynamic ++ taglist summaryProb
    where dynamic = [if summaryAccepted>0  then "*accepted*"
                     else "*not accepted*",
                     if summaryAttempts>0 then "*submitted*"
                     else "*not submitted*"]


{-
getAvailable :: UID -> ProblemSet -> AppHandler [ProblemSummary]
getAvailable uid probset@ProblemSet{..} 
    | probsetExam = do now <- liftIO getCurrentTime
                       filter (isAvailable now) <$> getProblemSummary uid probset
    | otherwise   = getProblemSummary uid probset
    where
      isAvailable :: UTCTime -> ProblemSummary -> Bool
      isAvailable now ProblemSummary{..} 
          = summaryAttempts>0 || now `Interval.elem` probOpen summaryProb 
-}

getProblemSummary :: UID -> ProblemSet -> AppHandler [ProblemSummary]
getProblemSummary uid ProblemSet{..} = do
  forM probsetProbs $ \prob -> do
    let pid = probID prob
    subs <- getSubmittedCount uid pid
    accepts <- getAcceptedCount uid pid
    return (ProblemSummary prob subs accepts)

{-
  subs <- getSubmissionsCount uid
  accs <- getAcceptedCount uid
  probs <- liftIO $ readProblemDir >>= mapM readProblem
  return [ SubmitSummary prob submits accepts 
           |  prob <- probs, let pid = probID prob,
           let submits = Map.findWithDefault 0 pid subs,
           let accepts = Map.findWithDefault 0 pid accs]
-}




