A test suite is a collection of test cases and / or results,
organized in a container with labelled partitions (such as a Map).
This module contains functions to build test suites for the SimpleCheck module,
and relies on the standard generators made available through the Testable class.
The test suite type used is a standard Map indexed by rank, called MapRankSuite.

\begin{code}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Test.GenCheck.System.TestSuite 
  ( GenInstruct
  , TestSuite
  , MapRankSuite
  , genSuite
  , testSuite
  , baseSuite
  , stdSuite
  , deepSuite
  , suiteMerge
  ) where

import Prelude hiding (map)
import qualified Prelude(map)
import Data.Map (Map)
import Data.Monoid(Monoid)
import System.Random(StdGen)

import Test.GenCheck.Base.Base (Rank, Count)
import Test.GenCheck.Generator.Generator (Generator, genTake, StandardGens(..))
import Test.GenCheck.Base.LabelledPartition as Partition (LabelledPartition(..))

\end{code}

The containers for the test cases must be instances of the LabelledPartition class.  
This class provides labelled partitions for the test cases (e.g. Data.Map), 
but also supports merging of the containers.  Multiple component test suites
can be merged into a single test suite using suiteMerge.

\begin{code}
type TestSuite c k v a = c k (v a) -- (LabelledPartition c k v, Monoid (v a), Ord k)

suiteMerge :: (LabelledPartition c k v, Monoid (v a), Ord k) => 
                  [TestSuite c k v a] -> TestSuite c k v a
suiteMerge = foldl1 Partition.merge

\end{code}

testSuite, genSuite and genPart build test suites from
generator(s) given a list of ranks and the number of values desired at that rank.
(called GenInstruct).  This provides a simple API for building test suites.

\begin{code}
type MapRankSuite a = TestSuite Map Rank [] a   -- = Map Rank [a]

type GenInstruct = [(Rank, Count)]

testSuite :: [Generator a] -> [GenInstruct] -> MapRankSuite a
testSuite gs rcs = suiteMerge $ zipWith genSuite gs rcs

genSuite :: Generator a -> GenInstruct -> MapRankSuite a
genSuite g rcs = suiteMerge $ Prelude.map (genPart g) rcs

genPart :: Generator a -> (Rank, Count) -> MapRankSuite a
genPart g (r,n) = Partition.new r $ genTake g r n
\end{code}

baseSuite is used for base type generators; all values are of rank 1.

\begin{code}
baseSuite :: StandardGens a -> Count -> MapRankSuite a
baseSuite (UnrankedGen g) n = 
  let gis =  [(1, n)]
  in  testSuite [g] (repeat gis)
baseSuite _ _ = error "baseSuite applied to a Ranked Generator"
\end{code}

stdSuite uses all of the standard generators to create a good heuristic
default approach for testing across multiple ranks.

\gordon{for now, this is the same as the deepSuite, just until I get to it???}

\begin{code}
stdSuite :: StandardGens a -> StdGen -> Rank -> Count -> MapRankSuite a
stdSuite (StdGens g1 g2 g3 g4) s rmax n = 
  let nr = ((n `div` (toInteger rmax) + 1) `div` 4) + 1
      gis =  [(r, nr) | r <- [1..rmax]]
      uni = fromInteger $ max (nr `div` 4 + 1) 3
  in  testSuite [g1, g2, g3 uni, g4 s] (repeat gis)
stdSuite (UnrankedGen _) _ _ _ = error "stdSuite applied to an Unranked Generator"
\end{code}

deepSuite performs a small number of tests over each rank up to a maximum size,
by dividing the number of tests evenly over the total number of ranks,
using the same rank and count instructions for each generator.

\begin{code}
deepSuite :: StandardGens a -> StdGen -> Rank -> Count -> MapRankSuite a
deepSuite (StdGens g1 g2 g3 g4) s rmax n = 
  let nr = ((n `div` (toInteger rmax) + 1) `div` 4) + 1
      gis =  [(r, nr) | r <- [1..rmax]]
      uni = fromInteger $ max (nr `div` 4 + 1) 3
  in  testSuite [g1, g2, g3 uni, g4 s] (repeat gis)
deepSuite (UnrankedGen _) _ _ _ = error "deepSuite applied to an Unranked Generator"
\end{code}

