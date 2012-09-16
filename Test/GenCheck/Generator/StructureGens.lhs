This module is a library of useful generators for structural types.
Additional generator modules should be added to the Generator directory.

\begin{code}
{-# LANGUAGE FlexibleInstances #-}

module Test.GenCheck.Generator.StructureGens 
  ( genListOf
  , genListAll
  , listStdGens
  , genTplAll
  ) where

import Test.GenCheck.Base.Base (Rank)
import Test.GenCheck.Generator.Enumeration (Label(..))
import Test.GenCheck.Generator.Generator (Generator, StandardGens(..), Testable(..))
import Test.GenCheck.Generator.BaseGens() -- Testable instances
import Test.GenCheck.Generator.Substitution (Structure(..), Structure2(..))

\end{code}

Lists are a special kind of structure to generate, since there is only one
possible list for each rank, and the substitution values are already in a list.

The genListOf combinator turns a generator of a type a into a generator of
lists of a's of the specified rank.  The rank of the resulting generator
defines the length of the lists of a's it generates.

The basic list generator, with unit as the sort, is given as genListAll.  The
Structure class is provided for completeness, but there is only one possible
list for any given rank.

\begin{code}
genListOf :: Generator a -> Rank -> Generator [a]
genListOf g r l = let xs = g r in subLs xs
  where subLs [] = []
        subLs xs@(_:_) = let (ys',yss) = splitAt l xs in ys' : (subLs yss)

instance Structure [] where
  substitute lxs ys = lsub lxs ys
    where 
       lsub [] zs          = (Just [], zs)
       lsub (_:_) []       = (Nothing, [])
       lsub (_:xs) (z:zs)  = 
            let (mlys', ys') = lsub xs zs
                mlys = maybe Nothing (\lys -> Just (z:lys)) mlys'
            in (mlys, ys')

-- remember that generating a list of rank 1 gives the empty list, not a list of one element   
genListAll :: Generator [Label]
genListAll r = [take (r-1) (repeat A)]

listStdGens :: StandardGens [Label]
listStdGens = StdGens g g (\_ -> g) (\_ -> g) False
  where g = genListAll

instance Testable [Label] where
  stdTestGens = listStdGens

listStdSub :: (Testable a) => StandardGens [a]
listStdSub = 
  let stdg = stdTestGens
  in StdGens (vector (genAll stdg 1)) (vector (genXtrm stdg 1))
             (\k -> (vector (genUni stdg k 1))) (\s -> (vector (genRand stdg s 1)))
             False
  where vector xs r = let (x, xs') = splitAt r xs in x : (vector xs' r)

instance Testable [Int] where
  stdTestGens = listStdSub :: StandardGens [Int]

\end{code}

A pair generator is two sorted, so is an instance of Structure2.
Pairs are always of rank 2.

\begin{code}
instance Structure2 (,) where
  substitute2 _ (x:xs) (y:ys) = (Just (x,y), xs, ys)
  substitute2 _ xs ys = (Nothing, xs, ys)
  
genTplAll :: Generator (Label, Label)
genTplAll r | r == 2    = [(A,B)]
genTplAll _ | otherwise = []

tplStdGens :: StandardGens (Label, Label)
tplStdGens = StdGens g g (\_ -> g) (\_ -> g) False where g = genTplAll

instance Testable (Label,Label) where
  stdTestGens = tplStdGens
\end{code}
