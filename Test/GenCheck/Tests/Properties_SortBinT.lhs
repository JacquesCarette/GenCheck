The SortBinT specification for the sorting the elements
of a binary tree in left to right ascending order.
The specification expects the following functions implemented:

\begin{verbatim}
BinTree a = BTNode a | BTBr (BinTree a) (BinTree a)

sortBinT:  sort the elements of the tree in ascending order without changing the shape 
biggest:   the largest element of a tree
smallest:  the smallest element of the tree
\end{verbatim}

The properties that must be satisfied are:

\begin{verbatim}
propSorted:  elements must be in ascending order after they are sorted
propShape:   the tree's shape does not change during sorting
\end{verbatim}

\begin{code}

module Test.GenCheck.Tests.Properties_SortBinT 
( propSorted
, propShape
, sorted -- wouldn't normally be exported, but I use this for a failing test
, sameShape -- wouldn't normally be exported
) where

import Test.GenCheck (Property)
import Test.GenCheck.Tests.SortBinTree(BinTree(..), sortBinTree, biggest, smallest)

propSorted :: (Ord a) => Property (BinTree a)
propSorted = sorted . sortBinTree

sorted :: (Ord a) => Property (BinTree a)
sorted (BTNode _) = True
sorted (BTBr tl tr) = ((biggest tl) <= (smallest tr)) && 
  (sorted tl) && (sorted tr)

propShape :: (Ord a) => Property (BinTree a)
propShape t = sameShape (t, sortBinTree t)

sameShape :: (Ord a) => Property ((BinTree a, BinTree a))
sameShape ((BTNode _), (BTNode _)) = True
sameShape ((BTBr l1 r1), (BTBr l2 r2)) = (sameShape (l1,l2)) && (sameShape (r1,r2))
sameShape _ = False

\end{code}



