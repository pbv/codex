{-
  Time intervals
-}

module Interval where

import Control.Applicative

-- intervals are pair of optional lower and upper bounds
data Interval t = Interval { start :: !(Maybe t), end :: !(Maybe t) } 
                  deriving (Eq, Ord, Show, Read)

instance Functor Interval where
    fmap f (Interval l u) = Interval (fmap f l) (fmap f u)

-- interval constructors
interval = Interval

after, before :: t -> Interval t
after t = Interval (Just t) Nothing
before t = Interval Nothing (Just t)

within :: Ord t => t -> t -> Interval t
within t1 t2 | t1<=t2 = Interval (Just t1) (Just t2)
             | otherwise = error "Interval.within: invalid arguments"

forever :: Interval t
forever = Interval Nothing Nothing

-- check that a time is within an interval
elem :: Ord t => t -> Interval t -> Bool
t `elem` (Interval (Just l) (Just u)) = l<=t && t<=u
t `elem` (Interval (Just l) Nothing ) = l<=t
t `elem` (Interval Nothing  (Just u)) = t<=u
t `elem` (Interval Nothing Nothing)   = True



-- extend an interval by a 
                
