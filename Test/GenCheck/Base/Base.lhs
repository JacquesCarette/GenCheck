The fundamental definitions for GenCheck.

Throughout GenCheck, structured types are indexed with a (Rank,Count) pair.
These types are used everywhere so are in Base.

\begin{code}
module Test.GenCheck.Base.Base
  ( Rank
  , Count
  , Property
  ) where

type Rank = Int
type Count = Integer

\end{code}

The property type is a function from the test domain to a Boolean, i.e. a
univariate proposition.  If the specification property has multiple arguments,
it must first be uncurried to make the test domain the product of the
arguments.

\begin{code}
type Property a = a -> Bool
\end{code}
