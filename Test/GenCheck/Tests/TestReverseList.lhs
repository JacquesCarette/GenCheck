\begin{code}
module Test.GenCheck.Tests.TestReverseList where

import System.Random (mkStdGen, StdGen)

import Test.GenCheck.Generator.Generator(Generator)

import Test.GenCheck
import Test.GenCheck.Generator.StructureGens (genListOf)
import Test.GenCheck.Generator.BaseGens(genBaseRangeAll, genIntRnd)
import Test.GenCheck.System.TestSuite (GenInstruct, deepSuite, genSuite)

import Test.GenCheck.PureTest (gcPureTest)

\end{code}

The properties that the revList function must satisfy:

propRevRevEq tests that revList is idempotent (reversing the reverse is the original);

propRevUnitEq ensures that the reverse of a singleton list is itself.

\begin{code}
propRevRevEq :: (Eq a) => Property [a]
propRevRevEq xs = (revList.revList) xs == xs

propRevUnitEq :: (Eq a) => Property a
propRevUnitEq x = let lx = [x] in revList lx == [x]

\end{code}

The implementation to test.

\begin{code}
revList :: [a] -> [a]
revList xs  = rev xs []
  where rev [] xs' = xs'
        rev (x:xs) xs' = rev xs (x:xs')

\end{code}

SimpleCheck will generate a collection of lists and evalute the property over them.
One or more generators must be provided to create the test cases.
Although the function and properties are polymorphic in the element type,
the actual test cases must be built with a concrete type.
Here we will use the exhaustive base type generator (genBaseRngAll)
taken from the Generator.BaseGens module to create unique lists of integers.
We also use the random integer generator (genIntRnd) to test the singleton list property;
note that this requires an Integer seed.

The genListOf combinator creates a generator for lists of Ints, 
where the length of the list is the generator rank
(all Int values are at rank 1).

\begin{code}

gRndInts s = genIntRnd s
gUniqueInts = genBaseRangeAll (1,100)
gIntLists = genListOf gUniqueInts 1

\end{code}

The test suite will be built from the generator given a set of generator instructions.
The instructions determine how many elements of each rank are generated for the test.
A collection of test generation strategies are included in the System.TestSuite module.
For the reverse idempotency, we use deepSuite to get a few test lists up to a maximum rank.

The suite for the singleton lists is built from the random integer generator.
All integers are rank 1, so no other ranks are tested.  Note that the random seed
is supplied here in this function, so the random values will always be identical
from test to test.  genSuite builds the test suite from a single generator

\begin{code}
testRevRev1 = 
  let ts = genSuite gIntLists [(r,1) | r <- [1..30]] 
  in gcPureTest propRevRevEq ts

testRevUnit1 = 
  let ts = genSuite (gRndInts (mkStdGen 54698735)) [(1, 100)]
  in gcPureTest propRevUnitEq ts

\end{code}

We also want to see a case fail, so lets try comparing reversing a list with the original.
\begin{code}
propRevFail :: (Eq a) => Property [a]
propRevFail xs =  revList xs == xs

testRevFail1 = 
  let lbl = "Testing reverse list = list, should fail to rank 5 with unique elements"
      ts =  genSuite gIntLists [(r,2) | r<-[1..5]]
  in gcPureTest propRevFail ts
\end{code}

