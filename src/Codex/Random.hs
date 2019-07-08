--
-- | A monad for computations that require pseudo-randomization
--
module Codex.Random(
  Rand,
  run, shuffle, choose, distinct,
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


-- | state monad for random computations
type Rand = State StdGen

run ::  Rand a -> Int -> a
run action salt = evalState action (mkStdGen salt)

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
fisherYatesStep ::
  RandomGen g => (IntMap a, g) -> (Int, a) -> (IntMap a, g)
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


-- | choose k distinct elements from a list
-- if k >= length, returns all the list elements
distinct :: Int -> [a] -> Rand [a]
distinct k xs = do
  let n = length xs
  ixs <- take k <$> shuffle [0..n-1]
  let xs' = [x | (i,x)<-zip [0..] xs, i`elem`ixs]
  return xs'


-- | choose one element from a list
choose :: [a] -> Rand a
choose [] = error "Rand.choose: empty list"
choose xs = do
  g <- get
  let (i,g') = randomR (0, length xs-1) g
  put g'
  return (xs!!i)
