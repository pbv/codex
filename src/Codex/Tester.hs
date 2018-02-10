
module Codex.Tester (
  Tester,
  testers,
  -- * module re-exports
  module Codex.Tester.Monad,
  module Codex.Tester.Result,
  module Codex.Tester.Utils,
  -- * generic stuff
  module Control.Monad
  ) where

import           Codex.Types(Code)
import           Codex.Tester.Monad
import           Codex.Tester.Result
import           Codex.Tester.Utils
import           Control.Applicative
import           Control.Monad 


-- | type synonym for an exercise tester
type Tester = Code -> Test Result

-- | combine a list of testers in sequence
testers :: [Tester] -> Tester 
testers []     _    = empty
testers (t:ts) code = t code <|> testers ts code
