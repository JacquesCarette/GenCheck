Generators produce lists of concrete (ground) terms of a given type.  They have
four objectives in the testing process:

\begin{itemize}
\item determine which elements of the type to instantiate,
\item provide a usable instance of the element (i.e. a ground term),
\item partition the set of values into finite subsets,
with each subset uniquely labelled by a natural number called the \emph{rank},
\item define an order in which to provide the elements of a given rank.
\end{itemize}

The meaning of the rank is dependent on the nature of the generated structure,
and there may be more than one acceptable definition of rank for a given data
type.  For example, the rank of a tree may be the number of leaves, or the
height of the tree, or the number of interior nodes.  The definition of the
rank needs to be clearly documented for each generator.  

For data structures, the rank is the number of data elements or nodes.  For
simple base types, the rank is 1; for more complicated base types such as
Double, a ranked generator may make sense. Any function from Rank (Int) to a
list of values of a type can be a generator in GenCheck compatible type systems
as long as it satisfies the ranking criteria.  

Generators might be constructed to:

\begin{itemize}
\item {embodying domain specific knowledge
e.g.physical addresses or international names,}
\item {improve efficiency for complicated structures,}
\item {incorporate value based constraints,}
\item {use incremental construction from smaller structures.}
\end{itemize}

\begin{code}

module Test.GenCheck.Generator.Generator 
( Generator
, generate
, genTake
, enumGenerator
, StandardGens(..)
, Testable(..)
, stdEnumGens
, enumGens
) where

import System.Random (StdGen)
import Data.List (genericTake)

import Test.GenCheck.Base.Base (Rank, Count)
import Test.GenCheck.Generator.Enumeration as 
            Enum(Enumeration, Enumerated(..), counter, getUnsafe,Label)
import Test.GenCheck.Generator.EnumStrat
\end{code}

The Generator type is defined to be a function from rank to a list of values.
Generators are not obliged to provide all possible values of a type, nor are
they obliged to provide only one occurence of the type in the list.  Although
those are useful properties for a generator to have, in some cases the
flexibility is desirable, such as random value generators.  Note that even
though the rank partitions the type into finite discrete subsets, there is no
restriction against duplicating values in the list, so the list may infinite.
Any function that satisfies the Generator type can be used with GenCheck
compatible systems, as long as they are total over non-negative ranks.

The generate function generates values of multiple ranks in a flat list.

\begin{code}
type Generator a = Rank -> [a]

generate :: Generator a -> [(Rank, Count)] -> [a]
generate g = concatMap $ uncurry (genTake g)

genTake :: Generator a -> Rank -> Count -> [a]
genTake g r n = genericTake n (g r)
\end{code}

Enumerated generators use an enumeration (Generator.Enumeration)  and an
enumerative strategy (a type independent list of indices, Generator.EnumStrat)
to order the generated values by mapping the selection order over the
enumeration.  The strategy is assumed to provide indices in the domain of the
enumeration, given the size of the enumeration at that rank.

\begin{code}
enumGenerator :: EnumStrat -> Enumeration c a -> Generator (c a)
enumGenerator strat e rnk =  fmap (Enum.getUnsafe e rnk) (strat (Enum.counter e rnk))
\end{code}

Four enumerative strategies form the standard approach to generating test
values, and these generators are grouped into the StandardGens structure for
use in the test programs and test suite building functions.
The standard generators are:

\begin{definition}
\item[allGen] an exhaustive generator
\item[xtrmGen] extreme / boundary condition generator
\item[uniGen]  picks a fixed number of uniformly separated elements
\item[randGen] random generator (infinitely many elements)
\end{definition}

The generators can be restricted to an interval of the total set of values
by providing a range of indices (low index, high index) into the enumeration.

A set of standard generators can be defined from the enumeration, (the random
generator requires an integer seed and uses the System.Random module) or
overridden with more efficient implementations.  The StandardGens structure is
just a convenient way to pass them around.

stdGenList fills in the uniform spacing provides the standard ordering in a
list format.

\begin{code}
class Show a => Testable a where
    stdTestGens  :: StandardGens a

data StandardGens a = StdGens 
  { genAll  :: Generator a
  , genXtrm :: Generator a
  , genUni  :: Int    -> Generator a
  , genRand :: StdGen -> Generator a
  }
  | UnrankedGen (Generator a)

enumGens :: Enumeration c Label -> StandardGens (c Label) 
enumGens e = StdGens allGen xtrmGen uniGen randGen
  where
    allGen   = enumGenerator exhaustG e
    xtrmGen  = enumGenerator extreme e
    uniGen   = \m' -> enumGenerator (uniform m') e
    randGen  = \s' -> enumGenerator (randG  s')  e

stdEnumGens :: Enumerated c => StandardGens (c Label)
stdEnumGens = enumGens enumeration
\end{code}
