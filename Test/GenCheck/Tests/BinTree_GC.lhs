This module is an example of the code that must be written or generated
for a data structure (BinT) to use the enumerative test case generators from GenCheck.
The main exports are the Structure and Enumeration instances,
but the external (stand alone) versions of those methods are also exported.

\begin{code}
{-# LANGUAGE FlexibleInstances,FlexibleContexts  #-}

module Test.GenCheck.Tests.BinTree_GC 
( substBinTree
, eBinTree
) where

import Control.Monad (liftM2)
import System.Random (mkStdGen)

import Test.GenCheck.Generator.Enumeration 
            (Enumeration, Enumerated(..), eProd, eSum, eNode, eConst, eMemoize)
import Test.GenCheck.Generator.Enumeration (Label(..))
import Test.GenCheck.Generator.Substitution (Structure(..))

import Test.GenCheck.Tests.SortBinTree (BinTree(..))

\end{code}

The composition functions work with instances of the Structure class.
This includes a substitution method for ``filling in'' elements of a structure.

\begin{code}
instance Structure BinTree where
  substitute = substBinTree

substBinTree :: BinTree a -> [b] -> (Maybe (BinTree b), [b])
substBinTree (BTNode _) [] = (Nothing, [])
substBinTree (BTNode _) (y:ys) = (Just (BTNode y), ys)
substBinTree (BTBr tx ty) ys = 
  let (mtx, ys') = substBinTree tx ys
      (mty, ys'') = substBinTree ty ys'
      br' = liftM2 BTBr mtx mty
  in (br', ys'')

\end{code}

The enumeration is used to count and select values of the structure
of the specified rank, which is the size of the structure.

\begin{code}
instance Enumerated BinTree where
  enumeration = eBinTree

eBinTree :: Enumeration BinTree Label
eBinTree = eMemoize $ e
  where e = eSum (eNode (BTNode A)) 
                 (eProd BTBr eBinTree eBinTree)

\end{code}


