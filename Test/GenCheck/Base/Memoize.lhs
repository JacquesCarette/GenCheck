Memoizing is a mechanism to get Haskell to store the results of a function
for use in later invocations of the function.
There are a number of different strategies for storing and retreiving prior results
which are preferred in different situations.
These strategies have been provided primarily for research into optimizing enumerations (Enumeration.lhs),
but might also be of use in other areas of GenCheck.

\begin{code}
module Test.GenCheck.Base.Memoize 
( memoize
, memoList
) where
\end{code}

The Memoize package from Hackage provides a general means for memoizing functions
over arbitrary algebraic datatypes.  This is exported again for convenience of comparison.

\begin{code}
import Data.Function.Memoize (memoize)

\end{code}

Probably the simplest memoization technique for (positive) integer maps 
is to map the function over the list of naturals,
and then use the input as an index into the list.
This is particularly effective for functions in which the value at $n$
is dependent only on the values $1$ through $(n-1)$.

\begin{code}

memoList :: (Int -> b) -> (Int -> b)
memoList f =  \x -> cache !! x where
              cache = map f [1..]

\end{code}
