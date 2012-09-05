Tests of pure code can be tested using a pure (non-monadic) test program.

\begin{code}
{-# LANGUAGE TypeFamilies #-}

module Test.GenCheck.PureTest
 ( gcPureTest
 , gcPureTestable
 , gcPureTestDeep
 , gcPureTestBase
 , GCResults
 , GCTestPt(..)
 ) where

import System.Random (StdGen)

import Test.GenCheck.Base.Base(Rank, Property)
import Test.GenCheck.Base.Datum
import Test.GenCheck.Base.Verdict
import Test.GenCheck.Generator.Generator (Testable(..))
import Test.GenCheck.System.TestSuite (deepSuite, stdSuite, baseSuite, MapRankSuite)
--import Test.GenCheck
\end{code}

gcPureTest maps the property over a test suite and returns 
the test results partitioned by rank.

gcPureTestable is the default pure test using the 
standard test suite and the default standard generators
for instances of the Testable class.

gcPureTestDeep is an alternative pure test using the 
``deep'' test suite and the default standard generators
for instances of the Testable class that are structures of limited ``width'',
such as binary trees.

gcPureTestBase is the default pure test for base (non-structure) types
using the standard test suite and the default standard generators
for instances of the Testable class.

\begin{code}
gcPureTest :: Show a => Property a -> MapRankSuite a -> GCResults a
gcPureTest p = fmap (Prelude.map (gcTestCase p))
  where
    gcTestCase :: Property a -> a -> GCTestPt a
    gcTestCase p' x = Pt (p' x) x

gcPureTestable :: (Testable a, Integral k)  => 
                        Property a -> StdGen -> Rank -> k -> GCResults a
gcPureTestable p s r n = 
  let ts = stdSuite stdTestGens s r (toInteger n)
  in  gcPureTest p ts

gcPureTestDeep :: (Testable a, Integral k)  => 
                        Property a -> StdGen -> Rank -> k -> GCResults a
gcPureTestDeep p s r n = 
  let ts = deepSuite stdTestGens s r (toInteger n)
  in  gcPureTest p ts

gcPureTestBase :: (Testable a, Integral k)  => Property a -> k -> GCResults a
gcPureTestBase p n = 
  let ts = baseSuite stdTestGens (toInteger n)
  in  gcPureTest p ts

\end{code}

GCResults holds the test results, a  labelled partition of GCTestPts, 
grouped by rank.

GCTestPt is a test case and result value; an instance of both Verdict and Datum,
allowing access to the test result and input value respectively.

\begin{code}
type GCResults a = MapRankSuite (GCTestPt a)
 
data GCTestPt a = Pt Bool a
instance Show a => Show (GCTestPt a) where
  show (Pt True x) = show x
  show (Pt False x) = "FAILED: " ++ (show x)

instance  Datum (GCTestPt a) where
  type DataType (GCTestPt a) = a
  datum (Pt _ x) = x  
instance Verdict (GCTestPt a) where
  verdict (Pt b _) = b

\end{code}

