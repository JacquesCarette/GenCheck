\section{Generator Library}

This module is a library of generators for standard base types.
Most of the generators here are created from strategies over enumerations,
called enumerative generators, but some are manually coded for efficiency.

\begin{code}
module Test.GenCheck.Generator.BaseGens where

import System.Random (StdGen,randomR, Random(),RandomGen(), randoms, randomRs)
import Math.Combinat (combine)
--import Data.Ratio (Ratio, (%))
import Numeric.IEEE 

import Test.GenCheck.Base.Base (Rank)
import Test.GenCheck.Generator.BaseEnum -- enum* base enumerations
import Test.GenCheck.Generator.EnumStrat
import Test.GenCheck.Generator.Generator (Generator, Testable(..), StandardGens(..))

\end{code}

Generic generator for Haskell enumerated base types.
Base type values are all rank 1; baseGen defines such a range.

Note that the exhaustive and random generators are implemented 
without using the base type enumerations, while the extreme and uniform generators 
apply the enumerative strategy to the underlying enumeration.  
Bypassing the enumeration avoids the overhead of the selection function, 
improving the performance of the generator.

\begin{code}
baseGen :: [a] -> Generator a
baseGen xs r = if r == 1 then xs else []

baseEnumGen :: EnumStrat -> BaseEnum a -> Generator a
baseEnumGen strat e r | r ==1 = map (getBaseUnsafe e) (strat (baseCount e))
baseEnumGen   _   _ _ | otherwise = []

baseEnumGCStdGens :: (EnumGC a) => StandardGens a
baseEnumGCStdGens = baseEnumGCGens base

baseEnumGCGens :: BaseEnum a -> StandardGens a
baseEnumGCGens e = StdGens allGen xtrmGen uniGen randGen True
  where
    allGen   = baseEnumGen exhaustG e
    xtrmGen  = baseEnumGen extreme e
    uniGen   = \m' -> baseEnumGen (uniform m') e
    randGen  = \s' -> baseEnumGen (randG  s')  e

genBaseRangeAll, genBaseRangeExt :: Enum a => (a,a) -> Generator a
genBaseRangeAll (l,u)   = baseGen [l..u]
genBaseRangeExt (l,u)   = baseEnumGen extreme (enumBaseRange (l,u))

genBaseRangeUni :: Enum a => (a,a) -> Int -> Generator a
genBaseRangeUni (l,u) k = baseEnumGen (uniform k) (enumBaseRange (l,u))

genBaseRangeRnd :: (RandomGen t, Random a) => (a,a) -> t -> Generator a
genBaseRangeRnd (l',u') t = baseGen $ rg (l',u') t 
  where 
    rg (l,u) s = let (x,s') = (randomR (l,u) s) in x : (rg (l,u) s')

genBaseStdGens :: (Enum a, Random a) => (a,a) -> StandardGens a
genBaseStdGens rng = StdGens (genBaseRangeAll rng) (genBaseRangeExt rng)
                             (genBaseRangeUni rng) (genBaseRangeRnd rng) True

instance Testable Int where
  stdTestGens = genBaseStdGens (-100, 100)

\end{code}

For bounded enumerative types, the minBound and maxBounds can be used.
The type needs to be provided explicitly, otherwise it would be ambiguous.
Copy and paste the functions as required for other enumerative bounded types,
or just use the ranged generators with (minBound, maxBound) as the argument.
These are the bounded Int generators.

\begin{code}

genIntAll, genIntExt :: Generator Int
genIntAll = baseGen [(minBound::Int)..(maxBound::Int)]
genIntExt = baseEnumGen extreme $ enumBaseRange ((minBound::Int), (maxBound::Int))

genIntUni :: Int -> Generator Int
genIntUni k = baseEnumGen (uniform k) $ enumBaseRange ((minBound::Int), (maxBound::Int))

genIntRnd :: RandomGen t => t -> Generator Int
genIntRnd t = baseGen $ rg ((minBound::Int), (maxBound::Int)) t 
  where rg (l,u) s = let (x,s') = (randomR (l,u) s) in x : (rg (l,u) s')

\end{code}

These are some Char and String generators. Note that the exhaustive generator
does not use the enumeration, but the other generators use the base type
enumeration and an enumerative strategy.

The Testable instance default generator for Char is the default character
range.

\begin{code}
instance Testable Char where
  stdTestGens = StdGens genDfltCharAll genDfltCharExt 
                        genDfltCharUni genDfltCharRnd True

genLowCharAll, genDfltCharAll, genUpperCharAll, genDigitCharAll :: Generator Char
genDfltCharAll     = baseGen [' '..'~']
genLowCharAll      = baseGen ['a'..'z']
genUpperCharAll    = baseGen ['A'..'Z']
genDigitCharAll    = baseGen ['0'..'9']

genLowCharRnd, genDfltCharRnd, genUpperCharRnd, genDigitCharRnd :: StdGen -> Generator Char
genDfltCharRnd   s = baseEnumGen (randG s) enumDfltChar
genLowCharRnd    s = baseEnumGen (randG s) enumLowChar
genUpperCharRnd  s = baseEnumGen (randG s) enumUpperChar
genDigitCharRnd  s = baseEnumGen (randG s) enumDigitChar

genLowCharExt, genDfltCharExt, genUpperCharExt, genDigitCharExt :: Generator Char
genDfltCharExt     = baseEnumGen extreme enumDfltChar
genLowCharExt      = baseEnumGen extreme enumLowChar
genUpperCharExt    = baseEnumGen extreme enumUpperChar
genDigitCharExt    = baseEnumGen extreme enumDigitChar

genLowCharUni, genDfltCharUni, genUpperCharUni, genDigitCharUni :: Int -> Generator Char
genDfltCharUni   k = baseEnumGen (uniform k) enumDfltChar
genLowCharUni    k = baseEnumGen (uniform k) enumLowChar
genUpperCharUni  k = baseEnumGen (uniform k) enumUpperChar
genDigitCharUni  k = baseEnumGen (uniform k) enumDigitChar

\end{code}

String (lists of character) generators where string length is the rank.
These could have been generated as list structures composed with characters,
but strings are used frequently so a more efficient implementation is desirable.

\begin{code}
genStrRangeAll :: (Char,Char) -> Generator String
genStrRangeAll (a,z) r = combine r [a..z]
genStrDfltCharAll    = genStrRangeAll (' ','~')
genStrLowCharAll     = genStrRangeAll ('a','z')
genStrDigitCharAll   = genStrRangeAll ('0','9')

genStrRangeRnd :: (Char,Char) -> StdGen -> Generator String
genStrRangeRnd (a,z) s r = 
   let (str,s') = bldStr (a,z) s r in str : (genStrRangeRnd (a,z) s' r)
genStrDfltRnd  = genStrRangeRnd (' ','~')
genStrLowRnd   = genStrRangeRnd ('a','z')
genStrDigitRnd = genStrRangeRnd ('0','9')

bldStr :: (Char,Char) -> StdGen -> Rank -> (String, StdGen)
bldStr (_,_) s 0 = ("",s)
bldStr (a,z) s r = 
  let (c,s') = randomR (a,z) s 
      (str,s'') = bldStr(a,z)  s' (r-1)
  in (c : str, s'')
 
\end{code}

Generating other scalars is a bit trickier.

Ratios: we can build enumerated generators of ratios,
or use the diagonalized counting of fractions to get a non-enumerated generator.

--\begin{code}
genRatioAll,genRatioXtrm :: Generator (Ratio Int)
genRatioAll = baseEnumGen exhaustG enumBasePosRatio
genRatioXtrm = baseEnumGen extreme enumBasePosRatio
genRatioRnd :: StdGen -> Generator (Ratio Int)
genRatioRnd s = baseEnumGen (randG s) enumBasePosRatio
genRatioUni :: Int -> Generator (Ratio Int)
genRatioUni n = baseEnumGen (uniform n) enumBasePosRatio

-- non-enumerated generator of positive ratios, Cantor counting (i.e. 1/1, 1/2, 2/1, 1/3, 2/2, 3/1, etc.)
diagInt :: Integral a => a -> [Ratio a]
diagInt n | n == 1 = [(1 % 1)]
diagInt n | n > 1 = [(i % (n-i+1)) | i<-[1..n]]
diagInt _ | otherwise = undefined

genPosRatio' :: (Num a, Eq a, Integral a1) => a -> [Ratio a1]
genPosRatio' r | r == 1 = foldr (++) [] $ map diagInt [1..]
genPosRatio' _ | otherwise = []

genRatio' :: (Num a, Eq a, Integral a1) => a -> [Ratio a1]
genRatio' r | r == 1 = let g = genPosRatio' (1::Integer) in 
                       interleave g $ map negate g
genRatio' _ | otherwise = undefined

--\end{code}

Doubles and Floats: hard code some sample generators, which include extreme
value generator, and a random generator.  Extreme values use the constants from
the IEEE754 Hackage.  There are no exhaustive generators for these values.

It would have been nice to make general RealFloat versions of all of these
generators, but Haskell couldn't handle the ambiguous typing.

\begin{code}

genDblRnd :: StdGen -> Generator Double
genDblRnd s r | r == 1 = (randoms s:: [Double])
genDblRnd _ _ | otherwise = ([] :: [Double])

genDblRangeRnd :: (Double,Double) -> StdGen -> Generator Double
genDblRangeRnd (l,u) s r | r == 1 = (randomRs (l,u) s :: [Double])
genDblRangeRnd   _   _ _ | otherwise = ([] :: [Double])

genDblXtrm :: Generator Double
genDblXtrm r | r == 1 = 
    [minNormal::Double, maxFinite::Double, negate (minNormal::Double), negate (maxFinite::Double), (0::Double),
     epsilon::Double, negate (epsilon::Double), infinity::Double, negate (infinity::Double), 
     nan::Double, (nanWithPayload (maxNaNPayload (1::Double)))::Double, (nanWithPayload 1)::Double]
genDblXtrm _ | otherwise = []

genDblUni :: Int -> Generator Double
genDblUni m r | (r==1) && (m > 2) = 
  let l = (negate maxFinite::Double)
      u = maxFinite::Double
      delta = (u / ((fromInteger.toInteger) (m-1))) - (l / ((fromInteger.toInteger) (m-1)))
  in take m $ iterate ((+) delta) l
genDblUni m r | r==1 && (m <= 2) = [ (negate maxFinite::Double), maxFinite::Double ]
genDblUni _ _ | otherwise = []

genDblRangeUni :: (Double,Double) -> Int -> Generator Double
genDblRangeUni (l,u) m r | (r==1) && (m > 2) = 
  let delta = (u / ((fromInteger.toInteger) (m-1))) - (l / ((fromInteger.toInteger) (m-1)))
  in take m $ iterate ((+) delta) l
genDblRangeUni (l,u) m r | r==1 && (m <= 2) = [l,u]
genDblRangeUni   _   _ _ | otherwise = []

genFltRnd :: StdGen -> Generator Float
genFltRnd s r | r == 1 = (randoms s:: [Float])
genFltRnd _ _ | otherwise = ([] :: [Float])

genFltRangeRnd :: (Float,Float) -> StdGen -> Generator Float
genFltRangeRnd (l,u) s r | r == 1 = (randomRs (l,u) s :: [Float])
genFltRangeRnd   _   _ _ | otherwise = ([] :: [Float])

genFltXtrm :: Generator Float
genFltXtrm r | r == 1 = 
    [minNormal::Float, maxFinite::Float, negate (minNormal::Float), negate (maxFinite::Float), (0::Float),
     epsilon::Float, negate (epsilon::Float), infinity::Float, negate (infinity::Float), 
     nan::Float, (nanWithPayload (maxNaNPayload (1::Float)))::Float, (nanWithPayload 1)::Float]
genFltXtrm _ | otherwise = []

genFltUni :: Int -> Generator Float
genFltUni m r | (r==1) && (m > 2) = 
  let l = (negate maxFinite::Float)
      u = maxFinite::Float
      delta = (u / ((fromInteger.toInteger) (m-1))) - (l / ((fromInteger.toInteger) (m-1)))
  in take m $ iterate ((+) delta) l
genFltUni m r | r==1 && (m <= 2) = [ (negate maxFinite::Float), maxFinite::Float ]
genFltUni _ _ | otherwise = []

genFltRangeUni :: (Float,Float) -> Int -> Generator Float
genFltRangeUni (l,u) m r | (r==1) && (m > 2) = 
  let delta = (u / ((fromInteger.toInteger) (m-1))) - (l / ((fromInteger.toInteger) (m-1)))
  in take m $ iterate ((+) delta) l
genFltRangeUni (l,u) m r | r==1 && (m <= 2) = [l,u]
genFltRangeUni   _   _ _ | otherwise = []
\end{code}
