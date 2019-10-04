{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
--
-- | A monad for computations that require pseudo-randomization
--
module Codex.Random(
  Rand,
  run, shuffle, choose, 
  ) where

import           System.Random
import           Control.Monad.State.Strict
import qualified Data.IntMap as IntMap
import           Data.IntMap (IntMap, (!))


-- | state monad for random computations
newtype Rand a
  = Rand { unRand :: State StdGen a }
  deriving (Functor, Applicative, Monad)

run :: Int -> Rand a -> a
run seed action  = evalState (unRand action) (mkStdGen seed)

-- | shuffle a list
shuffle :: [a] -> Rand [a]
shuffle xs = Rand $ do
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




-- | choose one element from a list
choose :: [a] -> Rand a
choose [] = error "Rand.choose: empty list"
choose xs = Rand $ do
  g <- get
  let (i,g') = randomR (0, length xs-1) g
  put g'
  return (xs!!i)

