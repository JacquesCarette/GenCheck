A partition of a set is a collection of disjoint subsets where 
every element of the set is in exactly one of these subsets.
The LabelledPartition class defines a labelled collection of sets, with an
internal container type for the element subsets (or parts), a label for
each part, and an external container that maps the labels to the parts.
LabelledPartitions with the same (label and container) type can be merged,
with a guarantee that no data is lost and that where labels are equal,
commonly labelled parts will be merged into a single part.

The standard set theoretic definition of partition requires that the parts be
non-empty.  For pragmatic convenience, this condition is relaxed in this module 
because of the labelling: the partition may contain a labelled empty part, 
which is semantically identical to the the label not being represented in the
partition.

The LabelledPartition class is a generalization of a Map over lists of
elements.  Since the LabelledPartition class overloads the names empty, insert,
map and fold, it is recommended that the module always be imported qualified, 
much like Data.Map.

\begin{code}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Test.GenCheck.Base.LabelledPartition 
( LabelledPartition(..)
, fromList
, relabel
, Test.GenCheck.Base.LabelledPartition.filter
) where

import Data.Map(Map)
import qualified Data.Map as Map
import Data.Monoid
import qualified Data.Foldable as Fold

\end{code}

The LabelledPartition class is parameterized over:
 c - the external container structure
 v - the internal part structure for the data values

It also uses the following:
 k - the labels for the internal containers
 a - the actual values in the container

In order to support merge, (v a) must form a monoid; 
v should generally be a free monoid not dependent on the contents of the
container.  There are exceptions: for example, the container might only retain
pass/fail information, in which case the (v a) might just be a generalized
boolean value under conjugation.

The LabelledPartition methods must obey the following rules:
   merge empty x                   == x           
   merge x empty                   == x
   merge empty (insert xs k y)     == merge xs (insert empty k y)
   merge (new k xs) (new k ys)     == new k (xs `mappend` ys)
   insert (k x (new k xs))         == new k (x : xs)  
   insert (k x (new k' xs))        == merge (new k [x]) (new k' xs)  for k /= k'
   lookup (insert empty k y) k     == Just y
   lookup (insert empty k y) k'    == Nothing for k' /= k
   lookup empty k'                 == Nothing

\begin{code}

class (Fold.Foldable (c k), Functor v) => LabelledPartition c k v where
  empty   :: c k (v a)
  new     :: (Ord k) => k -> [a] -> c k (v a)
  size    :: c k (v a) -> Int
  insert  :: (Ord k) => k -> a -> c k (v a) -> c k (v a)
  lookup  :: (Ord k) => k -> c k (v a) -> Maybe (v a)  
  merge   :: (Ord k, Monoid (v a)) => c k (v a) -> c k (v a) -> c k (v a)
  map     :: (Ord k) => (k -> a -> r) -> c k (v a) -> c k (v r)
  map f   = fold (\k x lys -> insert k (f k x) lys) empty
  fold    :: (k -> a -> b -> b) -> b -> c k (v a) -> b
  toList  :: c k (v a) -> [(k, v a)]

instance (LabelledPartition c k v, Ord k, Monoid (v r)) => Monoid (c k (v r)) where
  mempty  = empty
  mappend = merge

fromList :: (LabelledPartition c k v, Ord k, Monoid (v a)) => 
    [(k,[a])] -> c k (v a)
fromList = Fold.foldr (\(k,ys) -> merge (new k ys)) empty

\end{code}

The following functions can be used to relabel and filter values in a
partition.
\begin{description}
\item[relabel] reorganizes the container with different grouping labels,
\item[filter] extracts only the contents that satisfy a predicate into a new
container with the same labels.
\end{description}

\begin{code}
relabel :: (LabelledPartition c k v, LabelledPartition c k' v, Ord k') => 
    (k -> a -> k') -> c k (v a)-> c k' (v a)
relabel f cxs = fold (\k x -> insert (f k x) x) empty cxs

filter :: (LabelledPartition c k v, Ord k) => 
    (k -> a -> Bool) -> c k (v a) -> c k (v a)
filter p cxs = fold cp empty cxs where
  cp k x ys = if (p k x) then insert k x ys else ys

\end{code}

One possible LabelledPartition instance is a Haskell Map of lists.
It is necessary to override the default insertions and union methods
so that no values are overwritten during those operations.

Any Map of Monoids would also work, such as Maps of Maps,
with the merging of categories carried out with the |mappend|.

\begin{code}

instance (Ord k) => LabelledPartition Map k [] where
  empty      = Map.empty
  new k xs   = Map.fromList [(k,xs)]
  size       = Map.fold (\xs s -> (length xs) + s) 0
  insert k x = Map.insertWith mappend k [x]
  lookup     = Map.lookup
  merge      = Map.unionWith mappend
  fold f     = Map.foldrWithKey (\k x y -> foldr (f k) y x)
  toList     = Map.toList
\end{code}
