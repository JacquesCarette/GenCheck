The GenCheck class Structure and Enumeration instances for Zipper.
It is typical of what would be mechanically generated for the type,
but has been converted to literate Haskell and documented.
There is no special significance to the $_GC$ suffix in the name,
that is just a convention so the GenCheck instances module is easily found.

\begin{code}
{-# LANGUAGE FlexibleInstances,FlexibleContexts  #-}
module Test.GenCheck.Tests.ListZipper.ListZipper_GC where

import Control.Monad (liftM2)

import Test.GenCheck
import Test.GenCheck.Generator.Enumeration

--import Test.GenCheck.Generator.Generator (Testable(..))
--import Test.GenCheck.Generator.StructureGens()
--import Test.GenCheck.Generator.Substitution (Structure(..))

import Test.GenCheck.Tests.ListZipper.ListZipper

\end{code}

The Structure class has a single method called substitute.
It replaces the elements in the structure with elements from a list
and returns a newly populated instance of the structure and the remaining elements;
if there are insufficient elements in the list to populate the structure
Nothing is returned along with the original list of elements.

A default substitution method can be mechanically derived for algebraic data types,
by interpreting the type as a sum of products, and using substitute methods
for any substructures.  Alternate substitutions could be provided instead.

\begin{code}
instance Structure Zipper where
  substitute = substZipper

substZipper :: Zipper a -> [b] -> (Maybe (Zipper b), [b])
substZipper (Zip x1 x2) ys = 
  let (mx1, ys')  = substitute x1 ys
      (mx2, ys'') = substitute x2 ys'
      zip' = liftM2 Zip mx1 mx2
  in (zip', ys'')

\end{code}

The Zipper is just a product of two lists of like elements.
The enumeration is built from the combinators eProd and eList
(list gets a special combinator due to it's frequent appearance
and special syntax).  

The Enumeration consists of two functions that, given a rank,
produce the number of structures and a selector function that
generates a structure given an index.  These functions are highly recursive,
so are memoized using the Memoize Hackage (eMemoize just applies 
memoize to both the count and selector functions in the Enumeration records).

The selector function from the Enumeration creates instances of the structure.
The Label type is used as a way to instantiate those structures
and provide a label for the parametric type variables in the structure
as a convenience.

\begin{code}
instance Testable (Zipper Label) where
  stdTestGens = stdEnumGens

instance Enumerated Zipper where
  enumeration = eZipper

eZipper :: Enumeration Zipper Label
eZipper = eMemoize e
  where
    e = eProd Zip (eList A) (eList A)

\end{code}

\gordon{This should be in Test.GenCheck.Generator.Enumeration ???}

\begin{code}
eList :: a -> Enumeration [] a
eList x = mkEnum c s
  where c _ = 1  -- there is only one list of any length
        s r k | k == 1 = take r $ repeat x
        s _ _ | otherwise = error "Selector error"
\end{code}
