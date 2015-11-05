{-# LANGUAGE OverloadedStrings,  RecordWildCards #-}
{-  
   Get a submission summary for problems and worksheets:
   number of attempts and number of accepted submissions
-}
module Summary
    ( Summary(..)
    , getProblemSummary
    , getSummary
    ) where


import            Types
import            Problem 
import            Submission
import            Application
import            Control.Applicative

-- | submission summary for a problem
data Summary = Summary {
      summaryProb :: Problem,  -- the problem
      summaryTotal :: !Int,  -- total number of submissions
      summaryAccepted :: !Int   -- number of accepted submitions
    }

{-
instance Tagged ProblemSummary where
  taglist ProblemSummary{..} = dynamic ++ taglist summaryProb
    where dynamic = [if summaryAccepted>0  then "*accepted*"
                     else "*not accepted*",
                     if summaryAttempts>0 then "*submitted*"
                     else "*not submitted*"]
-}

getProblemSummary :: UID -> Problem -> Pythondo Summary
getProblemSummary uid prob@Problem{..} = do
  total <- getTotalSubmissions uid probID
  accepts<- getSubmitCount Accepted uid probID
  return (Summary prob total accepts)


getSummary :: UID -> Worksheet Problem -> Pythondo (Worksheet Summary)
getSummary uid (Worksheet meta items) = Worksheet meta <$> mapM summary items
  where summary (Left blocks) = return (Left blocks)
        summary (Right prob) = Right <$> getProblemSummary uid prob
        
  

{-
getProblemSummary :: UID -> ProblemSet -> Pythondo [ProblemSummary]
getProblemSummary uid ProblemSet{..} = do
  forM probsetProbs $ \prob -> do
    let pid = probID prob
    total <- getTotalSubmitions uid pid
    accepts <- getSubmitCount Accepted uid pid
    return (ProblemSummary prob total accepts)
-}



