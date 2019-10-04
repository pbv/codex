
module Codex.Handlers where

import           Codex.Types (UserLogin,Page)
import           Codex.Submission.Types (Submission)
import           Text.Pandoc.Builder (Blocks)
import           Control.Applicative 

type ReqPath = FilePath

--
-- handlers for exercises 
-- parameterized by the application monad
--
data Handlers m = Handlers
  { handleView :: UserLogin -> ReqPath -> Page -> m ()
    -- ^ view an exercise
  , handleSubmit :: UserLogin -> ReqPath -> Page -> m ()
    -- ^ submit a solution
  , handleReport :: UserLogin -> ReqPath -> Page -> Submission -> m ()
    -- ^ view a submissoin report
  , handlePrintout :: UserLogin -> Page -> Submission -> m Blocks
    -- ^ produce submission printout (as a Pandoc fragment)
  }

instance Alternative m => Semigroup (Handlers m) where
  h <> h' = Handlers
            { handleView = \u r p -> handleView h u r p <|>
                                     handleView h' u r p
            , handleSubmit = \u r p -> handleSubmit h u r p <|>
                                       handleSubmit h' u r p
            , handleReport = \u r p s -> handleReport h u r p s <|>
                                       handleReport h' u r p s
            , handlePrintout = \u p s -> handlePrintout h u p s <|>
                                         handlePrintout h' u p s
                                       
            }


instance Alternative m => Monoid (Handlers m) where
  mempty = Handlers
           { handleView = const . const . const empty
           , handleSubmit = const . const . const empty
           , handleReport = const . const . const . const empty
           , handlePrintout = const .  const . const empty
           }


