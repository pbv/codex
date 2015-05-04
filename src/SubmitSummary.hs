{-# LANGUAGE OverloadedStrings,  RecordWildCards #-}
module SubmitSummary
    ( SubmitSummary(..)
    , getSubmitSummary
    , summaryTags
    , allTags
    , submitAvailable
    ) where


import            Types
import            Problem
import            Submission
import            Application
import           Control.Monad.State

import qualified  Interval as Interval
import qualified  Data.Set as Set
import qualified  Data.Map as Map
import            Data.Time.Clock

-- | submission summary for a problem
data SubmitSummary = SubmitSummary {
      -- summaryPID  :: PID,    -- the problem's id
      summaryProb :: Problem, -- the problem
      summarySubmits :: !Int,  -- total number of submissions
      summaryAccepted :: !Int -- number of accepted submitions
    }



getSubmitSummary :: UID -> AppHandler [SubmitSummary]
getSubmitSummary uid = do
  subs <- getSubmissionsCount uid
  accs <- getAcceptedCount uid
  probs <- liftIO $ readProblemDir >>= mapM readProblem
  return [ SubmitSummary prob submits accepts 
           |  prob <- probs,
           let submits = Map.findWithDefault 0 (probID prob) subs,
           let accepts = Map.findWithDefault 0 (probID prob) accs]


submitAvailable :: UTCTime -> SubmitSummary -> Bool
submitAvailable now SubmitSummary{..} 
    = summarySubmits>0 || now `Interval.elem` probOpen summaryProb 



-- collect all tags for a problem (including dynamic)
summaryTags :: SubmitSummary -> [ProblemTag]
summaryTags SubmitSummary{..} = dynamic ++ probTags summaryProb
    where dynamic = [if summaryAccepted>0 then "*accepted*" else "*not accepted*",
                     if summarySubmits>0 then "*submitted*" else "*not submitted*"]

-- collect all tags 
allTags :: [SubmitSummary] -> [ProblemTag]
allTags = (dynamic ++) . Set.toList . Set.fromList . concatMap (probTags . summaryProb)
    where dynamic = ["*accepted*", "*not accepted*", "*submitted*", "*not submitted*"]

