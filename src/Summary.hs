{-# LANGUAGE OverloadedStrings,  RecordWildCards #-}
{-  
   Get a submission summary for a problem:
   number of attempts and number of accepted submissions
-}
module Summary
    ( ProblemSummary(..)
    , getProblemSummary
    ) where


import            Types
import            Problem 
import            Submission
import            Application
import            Control.Monad (forM)


-- | submission summary for a problem
data ProblemSummary = ProblemSummary {
      summaryProb :: Problem,  -- the problem
      summaryAttempts :: Int,  -- total number of submissions
      summaryAccepted :: Int   -- number of accepted submitions
    }


instance Tagged ProblemSummary where
  taglist ProblemSummary{..} = dynamic ++ taglist summaryProb
    where dynamic = [if summaryAccepted>0  then "*accepted*"
                     else "*not accepted*",
                     if summaryAttempts>0 then "*submitted*"
                     else "*not submitted*"]



getProblemSummary :: UID -> ProblemSet -> AppHandler [ProblemSummary]
getProblemSummary uid ProblemSet{..} = do
  forM probsetProbs $ \prob -> do
    let pid = probID prob
    subs <- getSubmissionCount uid pid
    accepts <- getAcceptedCount uid pid
    return (ProblemSummary prob subs accepts)





