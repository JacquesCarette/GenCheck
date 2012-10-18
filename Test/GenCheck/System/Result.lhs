A test result is a collective term for the individual result
of each test case and the overall verdict of pass or fail for the test.
The test result may also include certain details about the test,
in particular the input value of the test, but not necessarily.

GenCheck test systems work with a standard Result class, a partition (instance
of Partition) of the individual test results (instances of Verdict).  The
overall result is itself a verdict, the conjugate of the individual test
results.  Each part of the result is also a verdict and can be evaluated
separately.

The Partition instance provides a label/index for each part.
There is no constraint on  nature of this grouping or labelling; it may be a
trivial label for the entire result set.  This constraint allows report schemas
to assume at least one label is provided, even if it is just the title of the
whole test.

\begin{code}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Test.GenCheck.System.Result 
( dspVerdict
, dspSummary
, dspDetails
, DetailedResult(..)
, result
, resultPartial
) where

import Test.GenCheck.Base.Verdict(Verdict(..),SummaryVerdict(..))
import Test.GenCheck.Base.Datum as Datum( Datum(..))
import Test.GenCheck.Base.LabelledPartition as Partition(LabelledPartition(..))
import qualified Test.GenCheck.Base.LabelledPartition as Partition(filter)

\end{code}

The verdict of a set of tests is the conjugate of the individual Boolean results.
GenCheck test results are stored in a LabelledPartition,
and are instances of the Verdict class,
so the LabelledPartition is also a verdict,
and the following functions apply:

\begin{description}
\item [result] determine if all of the tests in the container passed
\item [resultPartial] determine if a subset of the tests all passed
\end{description}

LabelledPartitions are also naturally monoids, 
which allows partitions of partitions.

These two functions form an implicit Result class
for foldable containers of verdicts.

\begin{code}

result :: (LabelledPartition c k v, Verdict r) => c k (v r) -> Bool
result cxs = Partition.fold (\_ x y -> (verdict x) && y) True cxs

resultPartial :: (LabelledPartition c k v, SummaryVerdict v, Ord k, 
    Verdict r) => c k (v r) -> k -> Bool
resultPartial cxs k = maybe True summaryverdict $ Partition.lookup k cxs
\end{code}

The DetailedResult class represents partitions of test data and results.
It exposes the test values in the container (\emph{cases} method)
and allows just the failing cases to be extracted.
Any labelled partition of test data and results is 
an automatic instance of DetailedResult.

\begin{code}
class (Verdict r, Datum r) => DetailedResult c v r where
  cases    :: c (v r) -> c (v (DataType r))
  failures :: c (v r) -> c (v r)

instance (LabelledPartition c k v, Verdict r, Datum r, Ord k, Functor v, 
    Functor (c k)) => DetailedResult (c k) v r where
  cases    = fmap (\xs -> fmap Datum.datum xs)
  failures = Partition.filter (\_ x -> not (verdict x) )
\end{code}

Reporting functions print a summary of the result to the command line.
These are very primitive reporting functions and are included for convenience
and to serve as templates for other reporting functions.

\begin{code}
dspVerdict :: (LabelledPartition c k v, Verdict r, DetailedResult (c k) v r) => String -> c k (v r) -> IO (Bool)
dspVerdict lbl res =
  let fs = failures res
      n  = Partition.size res
      nf = Partition.size fs
  in do if (0 < length lbl) then putStrLn lbl else putStr ""
        if (nf == 0) then putStrLn ("PASSED " ++ show n ++ " cases.")
                     else putStrLn ("FAILED: " ++ show nf ++ " failed cases.")
        return $ result res

dspSummary :: (Datum r, Show k, Show (v (DataType r)), LabelledPartition c k v, 
               DetailedResult (c k) v r) => String -> c k (v r) -> IO ()
dspSummary lbl res = 
  let fs = failures res
      n  = Partition.size res
      nf = Partition.size fs
  in do if (0 < length lbl) then putStrLn lbl else putStr ""
        if (nf == 0) then putStrLn ("PASSED over " ++ show n ++ " cases.")
             else do putStrLn ("FAILED " ++ show nf ++ " Cases (of " ++ show n ++ "): " )
                     dspTestCases $ (cases fs)

dspDetails :: (Datum r, Show k, Show (v r), LabelledPartition c k v, 
               DetailedResult (c k) v r) => String -> c k (v r) -> IO ()
dspDetails lbl res = 
  let fs = failures res
      n  = Partition.size res
      nf = Partition.size fs
  in do if (0 < length lbl) then putStrLn lbl else putStr ""
        if (nf == 0) then putStrLn ("PASSED " ++ show n ++ " cases:")
             else do putStrLn ("FAILED: " ++ show nf ++ " cases (of " ++ show n ++ "): " )
        dspTestCases res        

dspTestCases :: (Show k, Show (v r), LabelledPartition c k v) => c k (v r) -> IO ()
dspTestCases res = 
  let res' = Partition.toList res
  in sequence_ $ Prelude.map dspPart res'
       where
          dspPart (r, xs) = do putStrLn $ "Rank / Size " ++ (show r) ++ ": " 
                               putStrLn $ show xs
                               putStrLn ""
\end{code}

