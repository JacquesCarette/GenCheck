\begin{code}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

{- |SimpleCheck provides a series of very simple test programs written against
the GenCheck framework.  The test suite is pulled from a single generator,
uses the |map| function to schedule the tests, and reports either pass or 
fail with the failing test cases.  Test cases and results are categorized by
rank, and
stored in a Map.

The functionality is similar to that of QuickCheck and SmallCheck, but the test
cases can generated using different strategies, including but not limited to
random and exhaustive generation. 

The naming convention for strategies is as follows:

    * the /std/ strategy first exhausts small test cases, combines some uniform
     and random sampling for &#8220;middle&#8221; sized cases, and 
     finally tests a small number of random cases up to rank 30.

    * the /deep/ strategy test a few structures of each rank up to rank 60.

    * the /base/ strategy tests /non-structure/ types (only!) where all values
     are of rank 1.

Each strategy also comes in 3 reporting styles, namely:

    * /Test/ will run all tests and report the number of failures

    * /Check/ will run all tests, but stop at the first failure

    * /Report/ runs all tests, and gives a complete report of what it did

There is a further set of options for each of the above, with /Args/
appended to the name, for further control.  See below for details.
-}

module Test.SimpleCheck (
-- * Simple top-level interface
-- ** Type
  Property
-- ** Default strategies with different reporting options
, stdTest
, stdReport
, stdCheck
, deepTest
, deepReport
, deepCheck
, baseTest
, baseReport
, baseCheck
-- ** With additional control parameters
, stdTestArgs
, stdReportArgs
, stdCheckArgs
, deepTestArgs
, deepReportArgs
, deepCheckArgs
, baseTestArgs
, baseReportArgs
, baseCheckArgs
-- * General interface for (simple) testing with output to terminal
, simpleTest
, simpleReport
, simpleCheck
-- * General interface for a pure (non-monadic) computation returning a result
, simplePure
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
generators (exhaustive, extreme, uniform and random) to create the test suites
for a property.

If the property input type is an instance of Testable, there is a default
implementation of the test that sets an arbitrary maximum rank for the test
cases and uses the standard generators from Testable.

stdTestArgs and deepTestArgs expose the maximum rank parameter and take the
standard generators as arguments so alternative generators can be provided (the
heuristic still assumes they are exhaustive, boundary, etc.).  They also
include a label for the test reporting.

The stdReport, deepReport, etc. test schedulers are the same, but they print
out all of the test cases, highlighting the failure cases.

\begin{code}
stdTest,stdReport,stdCheck,deepTest,deepReport,deepCheck :: 
    (Testable a, Integral k)  => Property a -> k -> IO ()
stdTest p n = stdTestArgs stdTestGens "" 30 p (toInteger n)
stdReport p n = stdReportArgs stdTestGens "" 30 p (toInteger n)
stdCheck p n = stdCheckArgs stdTestGens "" 30 p (toInteger n)
deepTest p n = deepTestArgs stdTestGens "" 60 p (toInteger n)
deepReport p n = deepReportArgs stdTestGens "" 60 p (toInteger n)
deepCheck p n = deepCheckArgs stdTestGens "" 60 p (toInteger n)

stdTestArgs,stdReportArgs,stdCheckArgs,deepTestArgs,
    deepReportArgs,deepCheckArgs :: Show a => 
        StandardGens a -> String-> Rank -> Property a -> Count -> IO ()
stdTestArgs  gs lbl r p n = 
  do s <- System.Random.newStdGen
     simpleTest lbl p (stdSuite gs s r n)
stdReportArgs  gs lbl r p n = 
  do s <- System.Random.newStdGen
     simpleReport lbl p (stdSuite gs s r n)
stdCheckArgs  gs lbl r p n = 
  do s <- System.Random.newStdGen
     simpleCheck lbl p (stdSuite gs s r n)

deepTestArgs  gs lbl r p n = 
  do s <- newStdGen
     simpleTest lbl p (deepSuite gs s r n)
deepReportArgs  gs lbl r p n = 
  do s <- newStdGen
     simpleReport lbl p (deepSuite gs s r n)
deepCheckArgs  gs lbl r p n = 
  do s <- newStdGen
     simpleCheck lbl p (deepSuite gs s r n)

baseTest,baseReport,baseCheck :: (Testable a, Integral k) => Property a -> k -> IO ()
baseTest p n   = baseTestArgs stdTestGens "" p (toInteger n)
baseReport p n = baseReportArgs stdTestGens "" p (toInteger n)
baseCheck p n  = baseCheckArgs stdTestGens "" p (toInteger n)

baseTestArgs,baseReportArgs,baseCheckArgs  :: Show a => StandardGens a -> String -> Property a -> Count -> IO ()
baseTestArgs gs lbl p n   = simpleTest lbl p (baseSuite gs n)
baseReportArgs gs lbl p n = simpleReport lbl p (baseSuite gs n)
baseCheckArgs gs lbl p n  = simpleCheck lbl p (baseSuite gs n)
\end{code}

simpleTest takes the test suite as input and evaluates the property at 
all of the test values, with just the failures reported.

SimpleTestPure just maps the property evaluation across a ranked |Map| of input
values to produce a ranked map of |SimpleTestPt| results.

\begin{code}
simpleTest, simpleReport, simpleCheck ::  Show a =>
           String -> Property a -> MapRankSuite a -> IO ()
simpleTest   lbl p ts = dspSummary lbl $ simplePure p ts
simpleReport lbl p ts = dspDetails lbl $ simplePure p ts
simpleCheck lbl p ts  = simpleCheckArgs lbl p ts 1

simplePure :: Show a => Property a -> MapRankSuite a -> SimpleResults a
simplePure p = fmap (Prelude.map (simpleTestCase p))
\end{code}

SimpleCheck runs a monadic test that terminates when finding the first failure.
SimpleCheckArgs allows the maximum rank and number of failure cases before termination to be set.

\begin{code}
simpleCheckArgs ::  Show a => String -> Property a -> MapRankSuite a -> Int -> IO ()
simpleCheckArgs lbl p ts _ = 
  do putStrLn "SimpleCheck not yet enabled, running all cases using simpleTest"
     dspSummary lbl $ simplePure p ts

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
