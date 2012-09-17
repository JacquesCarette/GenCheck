The generation of algebraic data structures is split into two steps.  First,
structure generators create instances of the structure type where the element
nodes are filled with a constant value, or a constant label for multi-sorted
structures.  These can be considered ``templates'' with ``holes'' for data
elements.  New generators are built by substituting elements, extracted from a
generator, into these templates to create one or more copies of each structure.
These new generators use the substitution method from the Structure class to
integrate the elements into the structures; the structures must be an instance
of the Structure class, or Structure2, etc. for multi-sorted structures.

The substitution functions use a structure generator and an element generator,
with each node (or ``hole'') in the template filled with a single value.
Elements are assumed to be base type generators, meaning they have no rank.
The different substitution functions produce differing numbers of copies of
each structure ``template'' to provide some flexibility in building generators.

Note that structures can be composed with other structures using the
Composition module, which takes into account the rank of both structure types.
These compound structures can themselves be filled using the substitution
functions.

\begin{code}

module Test.GenCheck.Generator.Substitution
( Structure(..)
, subst
, substN
, substAll
-- , subPerm
-- , subComb
, substStdGenN
, substStdGenAll
, substStdGenStd
-- , subStdGenPerm
-- , subStdGenComb
, Structure2(..)
, subst2
, subst2N
, subst2StdGen
-- , subst2StdGenN
-- , subst2StdGenAll
-- , subst2StdGenPerm
-- , subst2StdGenComb
, Structure3(..)
) where

import Data.Maybe (catMaybes)
import System.Random (split)

import Test.GenCheck.Generator.Generator (Generator, StandardGens(..))
\end{code}

The Structure class defines the \emph{substitution} method for a structure, the
mechanical details of replacing ``holes'' in the data structure with elements.
This is an order based operation with nodes populated from a list of elements.
There are two methods provided, one that assumes that enough elements are
available and throws an error if that is not so, the other returns a Maybe
encapsulated value and any leftover elements from the input list.

Instances of Structure can be generated mechanically from the data constructor
definitions via Template Haskell.

\begin{code}
class Structure c where
  substitute    :: c a -> [b] -> (Maybe (c b), [b])

class Structure2 c where
  substitute2   :: c a b -> [a'] -> [b'] -> (Maybe (c a' b'), [a'], [b'])

class Structure3 c where
  substitute3 :: c a1 a2 a3 -> [a1'] -> [a2'] -> [a3'] 
                       -> (Maybe (c a1' a2' a3'), [a1'], [a2'], [a3'])

\end{code}

\begin{description}
\item[subst] populate one copy of each generated structure
\item[substN] populate n copies without reusing the generated elements 
                 (does not guarantee unique values of those elements)
\item[substAll] populates each instance of the structure of a given rank
        with the same list of elements, and then all shapes with the next list
        until exhausting the element generator (non-terminating for infinite generators)
\end{description}

These three combinators all assume that the elements are of rank 1, regardless
of the rank of the structure.  This is typical of a structure with base type
nodes.

\begin{code}
subst :: Structure c => Generator (c a) -> Generator b -> Generator (c b)
subst gfx gy r =  
  let fxs = (gfx r)
      ys  = gy 1
  in gsub fxs ys

gsub :: Structure c => [c a] -> [b] -> [c b]
gsub [] _       = []
gsub (fx:fxs) ys = 
   let (mfy, ys') = substitute fx ys
   in case mfy of
        Nothing -> []
        Just fy -> fy : (gsub fxs ys')

substN :: Structure c => Int -> Generator (c a) -> Generator b -> Generator (c b)
substN n gfx gy r = gsubN n n (gfx r) (gy 1)

gsubN :: Structure c => Int -> Int -> [c a] -> [b] -> [c b]
gsubN _ 0 _  _       = []
gsubN _ _ [] _       = []
gsubN n k fxs@(fx:fxss) ys = 
   let (mfy, ys') = substitute fx ys
   in if k > 1 then maybe [] (\fy -> fy : (gsubN n (k-1) fxs ys')) mfy
               else maybe [] (\fy -> fy : (gsubN n n fxss ys')) mfy 

substAll :: Structure c => Generator (c a) -> Generator b -> Generator (c b)
substAll gfx gy r = 
  let ys = gy 1
      fxs = gfx r
  in gsubAll fxs ys

gsubAll :: Structure c => [c a] -> [b] -> [c b]
gsubAll [] _ = []
gsubAll l@(fx:fxs) ys = 
   let (mfy, ys') = substitute fx ys  -- subst elements into first structure
       -- sub same ys into remaining list of structures, guaranteed finite
       fys = catMaybes $  map (fst.((flip substitute) ys)) fxs
   in maybe [] (\x -> x:(fys ++ (gsubAll l ys'))) mfy
\end{code}

The ``standard'' list of generators is exhaustive, extreme (boundary), uniform
and random.  Using the data type instead of the StandardGens class avoids all
sorts of type hassles, and locks the random seed and uniform sampling size into
the record.

\begin{code}
substStdGenN :: Structure c => Int -> StandardGens (c a) -> Generator b -> StandardGens (c b)
substStdGenN n (StdGens g1 g2 g3 g4 True) g = 
  let ga = substN n g1 g
      gx = substN n g2 g
      gu k = substN n (g3 k) g
      gr s = substN n (g4 s) g
  in StdGens ga gx gu gr True
substStdGenN _ _ _ = error "Can only substitute into ranked"

substStdGenAll :: Structure c => StandardGens (c a) -> Generator b -> StandardGens (c b)
substStdGenAll (StdGens g1 g2 g3 g4 True) g = 
  let ga = substAll g1 g
      gx = substAll g2 g
      gu k = substAll (g3 k) g
      gr s = substAll (g4 s) g
  in StdGens ga gx gu gr True
substStdGenAll _ _ = error "Can only substitute into ranked"

substStdGenStd :: Structure c => 
                  StandardGens (c a) -> StandardGens b -> StandardGens (c b)
substStdGenStd (StdGens ga1 gx1 gu1 gr1 True) (StdGens ga2 gx2 gu2 gr2 _) =
  let ga = substAll ga1 ga2
      gx = substAll gx1 gx2
      gu k = substAll (gu1 k) (gu2 k)
      gr s = let (s1,s2) = split s in substAll (gr1 s1) (gr2 s2)
  in StdGens ga gx gu gr True
substStdGenStd _ _ = error "Can only substitute into ranked"

\end{code}

Multi-sort structure substitution. This could be accomplished with multiple
steps of single sort substitution, but simultaneous substitution allows more
options.

Again, the elements are assumed to be of rank 1.

\begin{code}
subst2 :: Structure2 c => Generator (c a b) -> Generator a' -> Generator b' 
                             -> Generator (c a' b')
subst2 gfxy gx' gy' r =  
  let fxs = gfxy r
      xs  = gx' 1
      ys  = gy' 1
  in gsub2 fxs xs ys

gsub2 :: Structure2 c => [c a b] -> [a'] -> [b'] -> [c a' b']
gsub2 [] _ _       = []
gsub2 (fx:fxs) ys zs = 
   let (mfy, ys', zs') = substitute2 fx ys zs
   in maybe [] (\fy -> fy : (gsub2 fxs ys' zs')) mfy

subst2N :: Structure2 c => Int -> Generator (c a b) -> Generator a' 
    -> Generator b' -> Generator (c a' b')
subst2N n gfx gy gz r = gsub2N n 1 1 (gfx r) (gy 1) (gz 1)

gsub2N :: Structure2 c => Int -> Int -> Int -> [c a b] -> [a'] -> [b'] -> [c a' b']
gsub2N _ 0 _ _  _ _      = []
gsub2N _ _ _ [] _ _      = []
gsub2N _ _ 0 _  _ _      = []
gsub2N _ _ _ _ [] _      = []
gsub2N n k1 k2 fxs@(fx:fxss) ys zs = 
   let (mfy, ys', zs') = substitute2 fx ys zs
   in if n > k2 
      then maybe [] (\fy -> fy : (gsub2N n k1 (k2+1) fxs ys zs')) mfy
      else if n > k1 then maybe [] (\fy -> fy : (gsub2N n (k1+1) 1 fxs ys' zs')) mfy
                     else maybe [] (\fy -> fy : (gsub2N n 1 1 fxss ys' zs')) mfy

subst2StdGen :: Structure2 c => StandardGens (c a b) -> Generator a' -> Generator b'
                                  -> StandardGens (c a' b')
subst2StdGen (StdGens g1 g2 g3 g4 True) ga' gb' = 
  let ga = subst2 g1 ga' gb'
      gx = subst2 g2 ga' gb'
      gu k = subst2 (g3 k) ga' gb'
      gr s = subst2 (g4 s) ga' gb'
  in StdGens ga gx gu gr True
subst2StdGen _ _ _ = error "Can only substitute into ranked"
\end{code}
