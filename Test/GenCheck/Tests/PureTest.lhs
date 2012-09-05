GC test results are stored in a |GCTestPt| structure,
and include only the test case and boolean property value.
|GCTestPt| is an instance of both Verdict and Datum,
which allow access to the test result and input value respectively.

\begin{code}
{-# LANGUAGE TypeFamilies #-}

module Test.GenCheck.Tests.PureTest
 ( gcPureTest
 , gcPureTestable
 , GCResults
 , gcTestCase
 , GCTestPt(..)
 ) where

import System.Random (StdGen)

import Test.GenCheck.Base.Datum
import Test.GenCheck.Base.Verdict
import Test.GenCheck

-- The default pure test is the standard test suite of the requested test cases,
-- up to rank r, for instances of the Testable class.
gcPureTestable :: (Testable a, Integral k)  => 
      Property a -> StdGen -> Rank -> k -> GCResults a
gcPureTestable p s r n = 
  let ts = stdSuite stdTestGens s r (toInteger n)
  in  gcPureTest p ts

gcPureTest :: Show a => Property a -> MapRankSuite a -> GCResults a
gcPureTest p = fmap (Prelude.map (gcTestCase p))

type GCResults a = MapRankSuite (GCTestPt a)
 
gcTestCase :: Property a -> a -> GCTestPt a
gcTestCase p x = Pt (p x) x

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


\begin{code}
\end{code}