{-
  Time intervals
-}

module Interval
    (
     Interval,
     interval,
     empty,
     forever,
     within,
     after,
     before,
     elem,
     limited,
     start,
     end
    ) where

import Prelude hiding (elem)

-- intervals are pair of optional lower and upper bounds
data Interval t = Interval !(Maybe t) !(Maybe t)  
                | Empty
                  deriving (Eq, Ord, Show, Read)

instance Functor Interval where
    fmap f (Interval l u) = Interval (fmap f l) (fmap f u)
    fmap f Empty          = Empty

-- interval constructors
interval :: Maybe t -> Maybe t -> Interval t
interval = Interval 

within :: Ord t => t -> t -> Interval t
within t1 t2 | t1<=t2 = Interval (Just t1) (Just t2)
             | otherwise = error "Interval.within: invalid arguments"

empty :: Interval t
empty = Empty

forever :: Interval t
forever = Interval Nothing Nothing

limited :: Interval t -> Bool
limited Empty                 = False
limited (Interval _ Nothing)  = False
limited (Interval _ (Just _)) = True


-- end point projections 
start, end :: Interval t -> Maybe t
start Empty          = Nothing
start (Interval l _) = l

end Empty          = Nothing
end (Interval _ u) = u


-- comparisions with times
after, before :: Ord t => t -> Interval t -> Bool
t `after` Empty               = False
t `after` Interval _ Nothing  = False
t `after` Interval _ (Just u) = t>u

t `before` Empty              = False
t `before` Interval Nothing _ = False
t `before` Interval (Just l) _ = t<l

-- check that a time is within an interval
elem :: Ord t => t -> Interval t -> Bool
t `elem` Empty                        = False
t `elem` (Interval (Just l) (Just u)) = l<=t && t<=u
t `elem` (Interval (Just l) Nothing ) = l<=t
t `elem` (Interval Nothing  (Just u)) = t<=u
t `elem` (Interval Nothing Nothing)   = True


                
