{-# LANGUAGE GADTs, DeriveFunctor #-}

-- A free-monad DSL for queries to the submission database
module Query where

import           Prelude hiding (sum)
import qualified Prelude (sum)
import Data.Time
import Types
import Problem
import Submission

{-
-- free monad stuff
data Free f r = Free (f (Free f r)) | Pure r

instance (Functor f) => Monad (Free f) where
    return = Pure
    (Free x) >>= f = Free (fmap (>>= f) x)
    (Pure r) >>= f = f r
    
liftF :: (Functor f) => f r -> Free f r
liftF command = Free (fmap Pure command)
-}  

-- * the query DSL
data Query row result where
  -- * selection
  Select :: (row -> Bool) -> Query row ()
  -- * projection
  Project :: (row -> row') -> Query row' result -> Query row result
  -- * agregation
  Aggregate :: ([row] -> result) -> Query row result
  -- * grouping
  GroupBy :: (row -> row -> Bool) -> Query row ()
  -- * sequence
  Bind :: Query row result' -> (result' -> Query row result) -> Query row result
  -- * unit 
  Return :: result -> Query row result


instance Monad (Query row) where
  return  = Return
  (>>=)   = Bind 


select :: (row -> Bool) -> Query row ()
select = Select

project :: (row->row') -> Query row' result -> Query row result
project = Project

aggregate :: ([row]->result) -> Query row result
aggregate = Aggregate

groupBy :: (row -> row -> Bool) -> Query row ()
groupBy = GroupBy

group :: Eq row => Query row ()
group = groupBy (==)

count :: Query row Int
count = aggregate length

sum :: Num a => Query a a
sum = aggregate Prelude.sum


-- * submission data base rows
type Row = (UID, Problem UTCTime, Submission)

-- count the number of accepted submissions
ex1 :: Query Row Int
ex1 = project (\(u,p,s) -> fmap reportStatus (submitReport s)) $
      do select (== Just Accepted)
         count

-- count the number of students with accepted submissions
ex2 :: Query Row Int         
ex2 = do select (\(u,p,s) -> fmap reportStatus (submitReport s) == Just Accepted)
         project (\(u,p,s) -> u) $ do {group ; count}

{-
instance Functor DSL where
  fmap f (Select b next) = Select b (f next)
  fmap f (Project g cont) = Project g (f . cont)
  fmap f (Count g) = Count (f . g)

-- * the query free monad
type Query next = Free DSL next

select :: Bool -> Query ()
select b = liftF (Select b ())

project :: (Row -> a) -> Query a
project f = liftF (Project f id)

count :: Query Int
count = liftF (Count id)

equalUID uid = project (\(uid',_,_) -> uid'==uid)
-}

