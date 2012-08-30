More test cases for straight up Int generators.
Trying out the three base type generators.
Run test(x/r/u) n, where n is the number of test cases,
for extreme, random and uniform samples
(the uniform sample is hard coded at 20 for no particular reason).

The random test generator is very poor for this case,
and extreme is excellent.  

\begin{code}
module Test.GenCheck.Tests.Test_BaseInt (testIntLT100_fail) where

import Test.GenCheck
import Test.GenCheck.Generator.Enumeration(Enumerated(..))
import Test.GenCheck.Generator.BaseEnum (enumBaseInt)
import Test.GenCheck.Generator.BaseGens
import Test.GenCheck.Generator.Generator (Generator, StandardGens(..))
import Test.GenCheck.System.TestSuite(MapRankSuite, baseSuite)

import Test.GenCheck.Tests.PureTest

stdIntGens = 
  let rng = (-100,100) 
  in StdGens (genBaseRangeAll rng) (genBaseRangeExt rng) 
             (genBaseRangeUni rng) (genBaseRangeRnd rng)
             True -- unranked

propUnder100 :: Property Int
propUnder100 = (< 100)

-- Should fail when finding value 100
testIntLT100_fail n = 
  let ts = baseSuite stdIntGens n
  in  gcPureTest  propUnder100 ts

\end{code}
