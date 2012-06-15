An \emph{enumerative strategy} is a list of indices that will be used to
produce a list of values of an enumerated type. \emph{Enumerative} generators
are the image of the enumerative strategy mapped to an enumeration.
This ``strategy'' abstracts the approach of selecting and ordering the values 
of an enumerated type.

Strategies are a heuristic means of selecting values. The usefulness
of the resulting generator will depend on the circumstances.  Any
list of Integers can be used as an enumerative strategy, so these can be
combined or customized as required for a given test.

\begin{code}
module Test.GenCheck.Generator.EnumStrat where

import System.Random (StdGen, randomR)
import Data.List (genericTake)

import Test.GenCheck.Base.Base(Count)

type EnumStrat = Count -> [Count]  
\end{code}

Exhaustive testing includes all of the elements of the type.  The exhaustive
strategy has been included primarily for completeness and convenience.  Using
an enumerative strategy to construct an exhaustive generator will be
inefficient in general, so a non-enumerative generator might be a better
choice.

\begin{code}
exhaustG :: EnumStrat
exhaustG u | u > 0 = [1..u]
exhaustG _ | otherwise = []
\end{code}

Random testing makes use of Haskell's Random module to provide infinite streams
of uniformly distributed indices.  It requires a seed on input.

Random testing may generate many duplicate or clustered test cases, providing
insufficient diversity in a test suite, and is more expensive to calculate than
the other strategies.  It is, however, a good tool for supplementing the other
strategies by providing some deviation from the uniform intervals in the index
selection.

\begin{code}
randG :: StdGen -> EnumStrat
randG s = \cnt -> if cnt>=0 then genericTake cnt $ gr s cnt else []
    where gr t cnt = let (x,s') = (randomR (1,cnt) t) in x : (gr s' cnt)
\end{code}

Create a uniform sampling of the types by partitioning the index interval into
equal increments.  The meaning of uniform is dependent on the structure and
it's enumeration, so this is more of a heuristic means of sampling the type
values, but it guarantees at least some coverage of the type.

If only 2 or fewer samples are requested, just the top and bottom of the
interval are chosen.

\begin{code}
uniform :: Int -> EnumStrat
uniform n u | n > 2 && u > 1 = 
  let s = (toRational u) / (toRational n)
  in if u > (toInteger n) then take (n+1) $ 1 : (map round (iterate ((+) s) s))
              else exhaustG u
uniform _ u | u > 1 = [1,u]
uniform _ u | u == 1 = [1]
uniform _ _ | otherwise = []

\end{code}

Extreme covers the boundaries of the range, then recursively splits the range
and takes those boundaries. For example,
    
|extreme (1,20) = [1,20,10,2,11,9,19,5,15,3,12,6,16,4,14,8,18,7,13,17]|

The first two extreme binary trees will be the ``all left'' and ''all right''
branched trees, which is a good start for testing, but in general extreme
indices may or may not map to boundary conditions in a particular structure.

Extreme generators is best used for pulling a small number of cases in
conjunction with random testing. It is not an efficient strategy to produce a
large number of test cases, and may contain duplicate index entries.

\begin{code}
extreme :: EnumStrat
extreme up | up == 0 = []
extreme up | up == 1 = [1]
extreme up | up > 1 = xtrm (1,up)
  where
    xtrm (l, u) = 
      let dif = u - l
          m = (dif `div` 2) + l
      in if dif < 1 then [l]
         else if dif < 2 then [l,u]
         else l : u : m :
           (if dif < 3 then [] 
               else interleave (xtrm ((l+1), (m-1))) (xtrm ((m+1), (u-1))))
extreme _ = error "extreme called with argument < 0"

interleave :: [a] -> [a] -> [a]
interleave xs [] = xs
interleave [] ys = ys
interleave (x:xs) (y:ys) =  x : (y : interleave xs ys)
\end{code}
