--
-- | A monad for random computations
--
module Codex.Handlers.Quiz.Random(
  Rand,
  runRand, shuffle,
  -- module System.Random.TF.Gen,
  -- module System.Random.TF.Init,
  -- module System.Random.TF.Instances
  module System.Random
  ) where

{-
import           System.Random.TG.Gen
import           System.Random.TF.Init
import           System.Random.TF.Instances
-}
import           System.Random
import           Control.Monad.State.Strict
import qualified Data.IntMap as IntMap
import           Data.IntMap (IntMap, (!))


-- | random shuffleing for questions & answers
-- monad for random computations
type Rand = State StdGen

runRand ::  Rand a -> Int -> a
runRand action salt = evalState action (mkStdGen salt)


-- | shuffle a list
shuffle :: [a] -> Rand [a]
shuffle xs = do
  g <- get
  let (xs', g') = fisherYates g xs
  put g'
  return xs'

--
-- | purely functional Fisher-Yates shuffling; O(n * log n) complexity
--
fisherYatesStep :: RandomGen g => (IntMap a, g) -> (Int, a) -> (IntMap a, g)
fisherYatesStep (m, gen) (i, x)
  = ((IntMap.insert j x . IntMap.insert i (m ! j)) m, gen')
  where
    (j, gen') = randomR (0, i) gen
 
fisherYates :: RandomGen g => g -> [a] -> ([a], g)
fisherYates gen [] = ([], gen)
fisherYates gen (x:xs) = 
  toElems $ foldl fisherYatesStep (initial x) (zip [1..] xs)
  where
    toElems (x, y) = (IntMap.elems x, y)
    initial x = (IntMap.singleton 0 x, gen)  
