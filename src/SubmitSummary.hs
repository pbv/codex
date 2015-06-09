{-# LANGUAGE OverloadedStrings,  RecordWildCards #-}
module SubmitSummary
    ( SubmitSummary(..)
    , getAvailable
    --, getSubmitSummary
    , summaryTags
    , allTags
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
data SubmitSummary = SubmitSummary {
      summaryProb :: Problem, -- the problem
      summaryAttempts :: Int,  -- total number of submissions
      summaryAccepted :: Int  -- number of accepted submitions
    }


getAvailable :: UID -> ProblemSet -> AppHandler [SubmitSummary]
getAvailable uid probset@ProblemSet{..} 
    | probsetExam = do now <- liftIO getCurrentTime
                       filter (isAvailable now) <$> getSubmitSummary uid probset
    | otherwise   = getSubmitSummary uid probset
    where
      isAvailable :: UTCTime -> SubmitSummary -> Bool
      isAvailable now SubmitSummary{..} 
          = summaryAttempts>0 || now `Interval.elem` probOpen summaryProb 


getSubmitSummary :: UID -> ProblemSet -> AppHandler [SubmitSummary]
getSubmitSummary uid ProblemSet{..} = do
  forM probsetProbs $ \prob -> do
    let pid = probID prob
    stats <- getSubmissions' uid pid
    let accepts = filter (==Accepted) stats
    return (SubmitSummary prob (length stats) (length accepts))

{-
  subs <- getSubmissionsCount uid
  accs <- getAcceptedCount uid
  probs <- liftIO $ readProblemDir >>= mapM readProblem
  return [ SubmitSummary prob submits accepts 
           |  prob <- probs, let pid = probID prob,
           let submits = Map.findWithDefault 0 pid subs,
           let accepts = Map.findWithDefault 0 pid accs]
-}




-- collect all tags for a problem (including dynamic)
summaryTags :: SubmitSummary -> [ProblemTag]
summaryTags SubmitSummary{..} = dynamic ++ probTags summaryProb
    where dynamic = [if summaryAccepted>0 then "*accepted*" else "*not accepted*",
                     if summaryAttempts>0 then "*submitted*" else "*not submitted*"]

-- collect all tags and remove duplicates
allTags :: ProblemSet -> [ProblemTag]
allTags probset = Set.toList $ Set.fromList $ dynamic ++ taglist probset
    where dynamic = ["*accepted*", "*not accepted*", 
                     "*submitted*", "*not submitted*"]

