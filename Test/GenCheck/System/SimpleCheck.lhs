\begin{code}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

{- |SimpleCheck is a series of very simple test programs written against the
GenCheck framework.  The test suite is pulled from a single generator, uses the
|map| function to schedule the tests, and reports either pass or fail with the
failing test cases.  Test cases and results are categorized by rank, stored in
a simple Map.

The functionality is similar to that of QuickCheck and SmallCheck, but the test
cases can generated using different strategies, including but not limited to
random and exhaustive generation. -}
module Test.GenCheck.System.SimpleCheck 
( simpleTest
, simpleReport
, simpleCheck
, stdTest
, stdReport
, stdCheck
, deepTest
, deepReport
, deepCheck
, baseTest
, baseReport
, baseCheck
, stdTestArgs
, stdReportArgs
, stdCheckArgs
, deepTestArgs
, deepReportArgs
, deepCheckArgs
, baseTestArgs
, baseReportArgs
, baseCheckArgs
) where

import System.Random (newStdGen)

import Test.GenCheck.Base.Base(Rank, Count, Property)
import Test.GenCheck.Base.Datum
import Test.GenCheck.Base.Verdict
import Test.GenCheck.Generator.Generator as 
            Generator (Testable(..),StandardGens(..))
import Test.GenCheck.System.TestSuite (deepSuite, stdSuite, baseSuite, MapRankSuite)
import Test.GenCheck.System.Result as Result(dspSummary, dspDetails)

-- passing through instances of Testable and Enumerated
import Test.GenCheck.Generator.StructureGens()
import Test.GenCheck.Generator.BaseGens()
import Test.GenCheck.Generator.BaseEnum()

\end{code}

The following test programs use a heuristic strategy over the standard
generators (exhaustive, boundary, uniform and random) to create the test suites
for a property.

If the property input type is an instance of Testable, there is a default
implementation of the test that sets an arbitrary maximum rank for the test
cases and uses the standard generators from Testable.

\begin{description}
\item [stdTest]  exhausts small test cases, combines uniform and random
sampling for ``middle'' size cases, and finally tests a small number of random
values up to rank 30
\item [deepTest] tests a few structures of each rank up to rank 60
\item [baseTest] tests base (non-structure) types where all values are of rank 1
\end{description}

stdTestArgs and deepTestArgs expose the maximum rank parameter and take the
standard generators as arguments so alternative generators can be provided (the
heuristic still assumes they are exhaustive, boundary, etc.).  They also
include a label for the test reporting.

The stdReport, deepReport, etc. test schedulers are the same, but they print
out all of the test cases, highlighting the failure cases.

\begin{code}
stdTest,stdReport,stdCheck :: (Testable a, Integral k)  => Property a -> k -> IO ()
stdTest p n = stdTestArgs stdTestGens "" 30 p (toInteger n)
stdReport p n = stdReportArgs stdTestGens "" 30 p (toInteger n)
stdCheck p n = stdCheckArgs stdTestGens "" 30 p (toInteger n)

stdTestArgs   :: Show a => StandardGens a -> String -> Rank -> Property a -> Count -> IO ()
stdTestArgs  gs lbl r p n = 
  do s <- System.Random.newStdGen
     simpleTest lbl p (stdSuite gs s r n)

stdReportArgs   :: Show a => StandardGens a -> String -> Rank -> Property a -> Count -> IO ()
stdReportArgs  gs lbl r p n = 
  do s <- System.Random.newStdGen
     simpleReport lbl p (stdSuite gs s r n)

stdCheckArgs   :: Show a => StandardGens a -> String -> Rank -> Property a -> Count -> IO ()
stdCheckArgs  gs lbl r p n = 
  do s <- System.Random.newStdGen
     simpleCheck lbl p (stdSuite gs s r n)

deepTest,deepReport,deepCheck :: (Testable a, Integral k)  => Property a -> k -> IO ()
deepTest p n = deepTestArgs stdTestGens "" 60 p (toInteger n)
deepReport p n = deepReportArgs stdTestGens "" 60 p (toInteger n)
deepCheck p n = deepCheckArgs stdTestGens "" 60 p (toInteger n)

deepTestArgs  :: Show a => StandardGens a -> String-> Rank -> Property a -> Count -> IO ()
deepTestArgs  gs lbl r p n = 
  do s <- newStdGen
     simpleTest lbl p (deepSuite gs s r n)

deepReportArgs  :: Show a => StandardGens a -> String-> Rank -> Property a -> Count -> IO ()
deepReportArgs  gs lbl r p n = 
  do s <- newStdGen
     simpleReport lbl p (deepSuite gs s r n)

deepCheckArgs  :: Show a => StandardGens a -> String-> Rank -> Property a -> Count -> IO ()
deepCheckArgs  gs lbl r p n = 
  do s <- newStdGen
     simpleCheck lbl p (deepSuite gs s r n)

baseTest,baseReport,baseCheck :: (Testable a, Integral k) => Property a -> k -> IO ()
baseTest p n = baseTestArgs stdTestGens "" p (toInteger n)
baseReport p n = baseReportArgs stdTestGens "" p (toInteger n)
baseCheck p n = baseCheckArgs stdTestGens "" p (toInteger n)

baseTestArgs  :: Show a => StandardGens a -> String -> Property a -> Count -> IO ()
baseTestArgs gs lbl p n = simpleTest lbl p (baseSuite gs n)

baseReportArgs  :: Show a => StandardGens a -> String -> Property a -> Count -> IO ()
baseReportArgs gs lbl p n = simpleReport lbl p (baseSuite gs n)

baseCheckArgs  :: Show a => StandardGens a -> String -> Property a -> Count -> IO ()
baseCheckArgs gs lbl p n = simpleCheck lbl p (baseSuite gs n)
\end{code}

simpleTest takes the test suite as input and evaluates the property at 
all of the test values, with just the failures reported.
simpleResult is the same but all of the test cases are reported.

SimpleTestRun just maps the property evaluation across a ranked |Map| of input
values to produce a ranked map of |SimpleTestPt| results.

\begin{code}
simpleTest, simpleReport, simpleCheck ::  Show a =>
           String -> Property a -> MapRankSuite a -> IO ()
simpleTest   lbl p ts = dspSummary lbl $ simpleTestRun p ts
simpleReport lbl p ts = dspDetails lbl $ simpleTestRun p ts
simpleCheck lbl p ts = simpleCheckArgs lbl p ts 1

simpleTestRun :: Show a => Property a -> MapRankSuite a -> SimpleResults a
simpleTestRun p = fmap (Prelude.map (simpleTestCase p))

\end{code}

SimpleCheck runs a monadic test that terminates when finding the first failure.
SimpleCheckArgs allows the maximum rank and number of failure cases before termination to be set.

\begin{code}
simpleCheckArgs ::  Show a => String -> Property a -> MapRankSuite a -> Int -> IO ()
simpleCheckArgs lbl p ts k = 
  do putStrLn "SimpleCheck not yet enabled, running all cases using simpleTest"
     dspSummary lbl $ simpleCheckRun p ts k

simpleCheckRun :: Show a => Property a -> MapRankSuite a -> Int -> SimpleResults a
simpleCheckRun p ste _ = simpleTestRun p ste
\end{code}

Simple test results are stored in a |SimpleTestPt| structure,
and include only the test case and boolean property value.
|SimpleTestPt| is an instance of both Verdict and Datum,
which allow access to the test result and input value respectively.

\begin{code}
type SimpleResults a = MapRankSuite (SimpleTestPt a)
 
simpleTestCase :: Property a -> a -> SimpleTestPt a
simpleTestCase p x = Pt (p x) x

data SimpleTestPt a = Pt Bool a
instance Show a => Show (SimpleTestPt a) where
  show (Pt True x) = show x
  show (Pt False x) = "FAILED: " ++ (show x)

instance  Datum (SimpleTestPt a) where
  type DataType (SimpleTestPt a) = a
  datum (Pt _ x) = x  
instance Verdict (SimpleTestPt a) where
  verdict (Pt b _) = b
\end{code}
