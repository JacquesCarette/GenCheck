The Descriptor class is a collection of functions that reify the type
constructor, mirroring Haskell's type constructor algebra.  Different instances
of the descriptor class can be used to produce generic functions for specific
Haskell structures; for example it could provide the generic fmap
for functors.

This module includes a very generic means for creating enumerations of
regular polynomial types in Haskell. Rank is always assumed to be 
the size of the structure, i.e. the number of nodes or data elements in the structure.  
For structures with multisorted substitutions, the rank is 
the sum of the number of elements of each sort.  
The most general definition of rank is a positive natural number
that indexes partitions of the set of elements of the type,
and here may be more than one way to rank the elements of a type.

Note that while the enumerations can always be constructed in this fashion,
for any regular polynomial type, they are very inefficient in this form.
The results of the counting computations cannot be memoized because
the compiler cannot know that the calls are recursive. The number of 
the shapes of lower rank are continually recomputed for those of higher rank.  
More efficient enumeration modules are also provided, and this module is included
because it is the most general and uses the least code to produce the enumerations.

\gordon{ Have to provide correct name for Combstruct generator modules }

\begin{code}
module Test.GenCheck.Generator.Descriptor
( Descriptor(..)
) where 

import Test.GenCheck.System.GenCheck(Rank,Count)
import Test.GenCheck.Generator.Enumeration( 
    Enumeration(Enum), mkEnumeration, enumProd, enumProd3, interval, deref)

\end{code}

Instances of the Descriptor class are a combinator algebra that 
can be used to create generalized functions over regular polynomial types.
Multiple product and sum combinators are provided to better match 
the Haskell type constructor syntax, but also to allow higher dimensional 
products to be indexed ``fairly'', meaning without arbitrary bias 
based on the order of the elements in the tuple.

Note that this is a ``finally tagless'' kind of encoding, where we are
parametric over an arbitrary type constructor.  This gives a different
flavour of genericity than most other frameworks.

\gordon{ In categorical terms, Descriptor d creates a functor out of d over the
HASK category??? }

\begin{code}

class Descriptor d where
    const :: a -> d a
    node  :: (a -> b) -> a -> d b
    sum2  :: d a -> d a -> d a
    prod2 :: (a -> b -> c) -> d a -> d b -> d c
    prod3 :: (a -> b -> c -> e) -> d a -> d b -> d c -> d e

\end{code}

The Counter instance of the Descriptor class provides 
the total number of structures for each rank (size) of an enumeration
(where the rank is the number of elements in a structure).
The newtype with constructor is required to keep the recursion working.

\begin{code}

newtype Counter a = Ctr (Rank -> Count)
unCtr :: Counter t -> (Rank -> Count)
unCtr (Ctr x) = x

instance Descriptor Counter where
    const _          = Ctr $ cConst
    node  _ _        = Ctr $ cNode
    sum2    c1 c2    = Ctr $ cSum (unCtr c1) (unCtr c2)
    prod2 _ c1 c2    = Ctr $ cProd (unCtr c1) (unCtr c2)
    prod3 _ c1 c2 c3 = Ctr $ cProd3 (unCtr c1) (unCtr c2) (unCtr c3)
\end{code}

We are fundamentally univariate, but sizes need to account for the
'size' of the constructor.  Right now, this is only visible for
|cConst| below, but could, in theory, be visible everywhere.  We don't 
actually do that, and it's fine (explanation too long to fit here).

\begin{code}
cConst, cNode :: (Num a, Num b) => a -> b
cConst r = if r == 1 then 1 else 0
cNode  r = if r == 1 then 1 else 0
cSum :: Num a => (t -> a) -> (t -> a) -> (t -> a)
cSum c1 c2 r = c1 r + c2 r
cProd :: (Rank -> Count) -> (Rank -> Count) -> (Rank -> Count)
cProd c1 c2 r = sum $ map fst $ enumProd c1 c2 r
cProd3 :: (Rank -> Count) -> (Rank -> Count) -> (Rank -> Count) -> (Rank -> Count)
cProd3 c1 c2 c3 r = sum $ map fst $ enumProd3 c1 c2 c3 r
\end{code}

Structure enumerations are special instances of the descriptor class.
The sum and product combinators use irrefutable pattern matching
to avoid infinite constructions.

\begin{code}

instance Descriptor Enumeration where
    const = bConst
    node  = bNode
    sum2  = bSum
    prod2 = bProd
    prod3 = bProd3

bConst :: a -> Enumeration a
bConst x = mkEnumeration cConst nilb
  where nilb r k | ((r == 1) && (k==1)) = x 
        nilb _ _ | otherwise            = undefined::a

bNode :: (a->b) -> a -> Enumeration b
bNode con x = mkEnumeration cNode snglb
  where snglb r k | ((r == 1) && (k == 1)) = con x 
        snglb _ _ | otherwise              = undefined::b

bSum :: Enumeration a -> Enumeration a -> Enumeration a
bSum ~(Enum c1 g1) ~(Enum c2 g2) = mkEnumeration (cSum c1 c2) select
    where select r k = let c = c1 r in if k <= c then g1 r k else g2 r (k - c)

bProd :: (a->b->c) -> Enumeration a -> Enumeration b -> Enumeration c
bProd con ~(Enum c1 g1) ~(Enum c2 g2) = mkEnumeration (cProd c1 c2) select
    where select r k = let ([r1,r2],k') = interval k $ enumProd c1 c2 r
                           [k1,k2] = deref [c1 r1, c2 r2] k'
                       in con (g1 r1 k1) (g2 r2 k2)
            
bProd3 :: (a->b->c->d) -> Enumeration a -> Enumeration b 
                       -> Enumeration c -> Enumeration d
bProd3 con ~(Enum c1 g1) ~(Enum c2 g2) ~(Enum c3 g3) = mkEnumeration (cProd3 c1 c2 c3) select
    where select r k = let ([r1,r2,r3],k') = interval k $ enumProd3 c1 c2 c3 r
                           [k1,k2,k3] = deref [c1 r1, c2 r2, c3 r3] k'
                       in con (g1 r1 k1) (g2 r2 k2) (g3 r3 k3)
\end{code}

