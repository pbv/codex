{-# LANGUAGE OverloadedStrings,  RecordWildCards #-}
{-  
   Get a submission summary for problems and worksheets:
   number of attempts and number of accepted submissions
-}
module Summary
    ( 
     getWorksheetSubmissions
    ) where


import            Types
import            Problem
import            Submission
import            Application
import            Control.Applicative


{-
-- | submission summary for a problem
data Summary = Summary {
  problem :: Problem,
  total :: !Int,     -- total number of submissions
  accepted :: !Int   -- number of accepted submitions
  } 

getProblemSummary :: UserID -> Problem -> Pythondo Summary
getProblemSummary uid prob@Problem{..} = do
  total <- countSubmissions uid probID
  accepts<- countSubmissions' uid probID Accepted
  return (Summary prob total accepts)
-}

  

{-
getProblemSummary :: UID -> ProblemSet -> Pythondo [ProblemSummary]
getProblemSummary uid ProblemSet{..} = do
  forM probsetProbs $ \prob -> do
    let pid = probID prob
    total <- getTotalSubmitions uid pid
    accepts <- getSubmitCount Accepted uid pid
    return (ProblemSummary prob total accepts)
-}



