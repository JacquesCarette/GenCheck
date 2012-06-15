A verdict is a Boolean pass or fail 'verdict' on a test result; an instance of
the Verdict class evaluates to such a result, but may also contain additional
information about the test.

A collection of verdicts is also a verdict: if all of the results are ``pass'',
the verdict for the collection is pass.  Specifically, the collective verdict
is the Boolean conjunction of the individual verdicts.  Therefore any foldable
container of verdicts is also a verdict.

\begin{code}
{-# LANGUAGE FlexibleInstances #-}
module Test.GenCheck.Base.Verdict ( Verdict(..), SummaryVerdict(..) ) where

import Data.Foldable(Foldable())

class Verdict s where
  verdict :: s -> Bool

instance Verdict Bool where
  verdict = id
\end{code}

Structures can be evaluated to a boolean if they are foldable and their
elements are generic booleans.

\begin{code}
class Foldable s => SummaryVerdict s where
  summaryverdict :: Verdict v => s v -> Bool
\end{code}
