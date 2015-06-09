{-
  Time intervals
-}

module Interval
    (Interval,
     interval,
     empty,
     forever,
     within,
     after,
     before,
     elem, notElem,
     limited,
     start,
     end,
     union, intersect            
    ) where

import Prelude hiding (elem, notElem)

-- intervals are pairs of optional lower and upper bounds
data Interval t = Interval !(Maybe t) !(Maybe t)  
                | Empty
                  deriving (Eq, Ord, Show, Read)

instance Functor Interval where
    fmap f (Interval l u) = Interval (fmap f l) (fmap f u)
    fmap f Empty          = Empty


-- interval constructors
interval :: Ord t => Maybe t -> Maybe t -> Interval t
interval l r | l<=r = Interval l r 
             | otherwise = Empty


within :: Ord t => t -> t -> Interval t
within t1 t2 | t1<=t2 = Interval (Just t1) (Just t2)
             | otherwise = Empty

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
elem, notElem :: Ord t => t -> Interval t -> Bool
t `elem` Empty                        = False
t `elem` (Interval (Just l) (Just u)) = l<=t && t<=u
t `elem` (Interval (Just l) Nothing ) = l<=t
t `elem` (Interval Nothing  (Just u)) = t<=u
t `elem` (Interval Nothing Nothing)   = True

notElem time int = not (elem time int)


-- union and intersection
union, intersect :: Ord t => Interval t -> Interval t -> Interval t
union Empty int                       = int
union int Empty                       = int
union (Interval l u) (Interval l' u') = Interval (minB l l') (maxB u u')

intersect (Interval l u) (Interval l' u') = interval (maxB l l') (minB u u')
intersect Empty          _                = Empty
intersect _              Empty            = Empty


minB, maxB :: Ord t => Maybe t -> Maybe t -> Maybe t
minB Nothing _         = Nothing
minB _       Nothing   = Nothing
minB (Just a) (Just b) = Just (min a b)

maxB Nothing u         = u
maxB l       Nothing   = l
maxB (Just a) (Just b) = Just (max a b)


                
