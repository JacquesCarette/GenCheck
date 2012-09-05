Test the BinTree sort function against it's specification.
The sortBinT function takes a BinTree structure as input,
and returns a BinTree with the leaves sorted in a depth first left to right
ascending order by value (assumes the elements can be ordered).
The tests are run through the SimpleCheck test program,
with test suites generated from the standard generators over
the BinTree enumeration.


\begin{code}
{-# LANGUAGE FlexibleInstances #-}

module Test.GenCheck.Tests.SortBinTree.Test_SortBinT where

import System.Random (StdGen, mkStdGen, split)
import Control.Monad (liftM2)

import Test.GenCheck
import Test.GenCheck.Generator.EnumStrat
import Test.GenCheck.Generator.Generator (stdEnumGens, enumGenerator)
import Test.GenCheck.Generator.BaseGens
import Test.GenCheck.Generator.Substitution (substStdGenAll)

import Test.GenCheck.Tests.SortBinTree.SortBinTree
import Test.GenCheck.Tests.SortBinTree.BinTree_GC 
import Test.GenCheck.Tests.SortBinTree.Properties_SortBinT
import Test.GenCheck.Tests.PureTest

\end{code}


\begin{code}


\end{code}

The btGens(R/X) generators are the BinTree structure generators
composed with either random or boundary (extreme) integer (-100 to 100)
generators for elements.

The btGensR generators will be made the default for the Testable instance 
of binary trees with Int nodes.  The seed for the random node generator
must be baked into the default instance of Testable for use with the simpler stdTest,
but it can alternately be carried along with the generators to be populated 
in the IO monad of the main test program by using stdTestArgs.

\begin{code}
nodeIntRnd s = genBaseRangeRnd (-100,100) s
nodeIntXtrm = genBaseRangeExt (-100,100)


instance Testable (BinTree Int) where
    stdTestGens = btGensR (mkStdGen 49873568)

btGensR :: StdGen -> StandardGens (BinTree Int)
btGensR s = substStdGenAll (stdEnumGens :: StandardGens (BinTree Label)) (nodeIntRnd s) 


btGensX :: StandardGens (BinTree Int)
btGensX = substStdGenAll (stdEnumGens :: StandardGens (BinTree Label))  nodeIntXtrm


\end{code}

The tests are based on determining if the tree is sorted correctly
after being processed by sortBinT.

The sortBinT function turns out to be rather slow for large trees,
so testing on trees of rank greater than 25 turns out to be very slow,
but that is independent of the testing program.

\begin{code}

btgs =  StdGens allGen xtrmGen uniGen randGen False
  where
    allGen   = enumGenerator exhaustG eBinTree
    xtrmGen  = enumGenerator extreme eBinTree
    uniGen   = \m' -> enumGenerator (uniform m') eBinTree
    randGen  = \s' -> enumGenerator (randG  s')  eBinTree

testBTSort_RandNode s r n = 
    let lbl = "Test sorting with random integer node trees, up to size " ++ show r
        (s1,s2) = split s
        ts = stdSuite (btGensR s1) s2 r n 
    in gcPureTest propSorted ts

testBTSort_XtrmNode s r n = 
    let lbl = "Test sorting with boundary integer node trees, up to size " ++ show r
        ts = stdSuite btGensX s r n 
    in gcPureTest propSorted ts

\end{code}

The following tests should fail, as the trees aren't sorted first.
For small trees, the elements might be generated in sorted order by coincidence.

\begin{code}

failBTSort_RandNode s r n = 
    let lbl = "This should fail when an unsorted tree is found."
        (s1,s2) = split s
        ts = stdSuite (btGensR s1) s2 r n 
    in gcPureTest sorted ts

\end{code}
