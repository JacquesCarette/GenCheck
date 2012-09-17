Create generators and an instance of Testable for an adapted version
of th DecimalRaw data structure from the Decimal package.
The local copy of DecimalRaw drops the Integral constraint
from the data statement to avoid the need for the DatatypeContexts extension
and makes the precision a type variable to clearly identify it as 
a two sorted structure.

Two different approaches for generating decimals will be used.
The first uses substitution for both the precision and mantissa values.
The second creates a single enumeration for the entire set of decimal values,
and then standard generators over the entire set.

\begin{code}
{-# LANGUAGE FlexibleInstances #-}

module Decimal_GC where

-- import Data.Decimal
import Data.Word (Word8)
import System.Random (StdGen, split)

import Test.GenCheck (Structure(..), Structure2(..), subst2N, Label(..), 
                      Enumerated(..), Enumeration)
import Test.GenCheck.Generator.Enumeration (mkEnumeration)
import Test.GenCheck.Generator.BaseEnum
-- (BaseEnum, EnumGC(base), enumList, 
--   baseEnumGCStdGens     beMemoize, beProd)
import Test.GenCheck.Generator.Generator (Generator, stdEnumGens,
                      enumGens, StandardGens(..), Testable(..))
import Test.GenCheck.Generator.BaseGens (baseEnumGCGens,baseEnumGCStdGens)
import Test.GenCheck.Generator.Substitution( substStdGenStd )

\end{code}

The DecimalRaw structure represents a fixed precision decimal expansion,
stored as decimal places (precision) and a mantissa.  It is a two sorted structure,
so an instance of Structure2.
\begin{code}
data DecimalRaw w8 i = Decimal {  -- (Integral i) constraint dropped
      decimalPlaces :: ! w8,
      decimalMantissa :: ! i}
instance (Show w8, Show i) => Show (DecimalRaw w8 i) where
  show (Decimal p m) = (show m) ++ " x10^" ++ (show p)

instance Structure2 DecimalRaw where
  substitute2 = subDec
subDec :: DecimalRaw a b -> [j] -> [i] -> (Maybe (DecimalRaw j i), [j], [i])
subDec (Decimal _ _) (p:ps) (m:ms) = (Just (Decimal p m), ps, ms)
subDec _ ps ms = (Nothing, ps, ms)

\end{code}

There is only one possible Decimal structure - the labelled product Decimal _ _.
The generator for these structures is therefor a single value of rank 2;
the Label type provides constants to identify that the sorts are different.

\begin{code}
genDecStruct :: Generator (DecimalRaw Label Label)
genDecStruct r | r == 2    = [Decimal A B]
genDecStruct _ | otherwise = []
\end{code}

For testing, fix the precision as a Word8 type and
create a base enumeration from the list of values,
then define the standard GenCheck (base) generators.

\begin{code}
enumW8 = enumList ([minBound..maxBound]::[Word8]) 
stdGensW8 = baseEnumGCGens enumW8

\end{code}

Instead of fixing the mantissa type, require it to be an instance of EnumGC.
This guarantees a base enumeration is available to define standard generators.

To use Integral, an arbitrary range of Integers will be defined as 
the instance of EnumGC.

\begin{code}
enumInteger n = enumList ([(-n), n])
instance EnumGC Integer where
  base = enumInteger 100000

\end{code}

The full generators for populated DecimalRaw values are given by 
substituting the mantissa and precision generators into 
the Decimal structure generator.  The subst2N function creates
n x n instances of each two sort structure populated, in this
case 100 instances of the unique DecimalRaw structure.

The mantissa and precision generators have independent sampling strategies,
so the DecimalRaw generator can have hybrid sampling strategies.
The three below mix the random and extreme (boundary) strategies,
but that is arbitrary.

\begin{code}
genDecRndXtrm ::  (EnumGC i) => StdGen -> Generator (DecimalRaw Word8 i)
genDecRndXtrm s  = subst2N 10 genDecStruct (gW8r s) gix
   where gix      = genXtrm baseEnumGCStdGens
         gW8r s'  = genRand stdGensW8 s'

genDecXtrmRnd ::  (EnumGC i) => StdGen -> Generator (DecimalRaw Word8 i)
genDecXtrmRnd s  = subst2N 10 genDecStruct gW8x (gir s)
   where gir  s' = genRand baseEnumGCStdGens s'
         gW8x    = genXtrm stdGensW8

genDecRndRnd ::  (EnumGC i) => StdGen -> StdGen -> Generator (DecimalRaw Word8 i)
genDecRndRnd s1 s2 = subst2N 10 genDecStruct (gW8r s1) (gir s2)
   where gir  s'   = genRand baseEnumGCStdGens s'
         gW8r s'   = genRand stdGensW8 s'
\end{code}

As an alternative, we can define an enumeration over 
the entire set of precisions for the DecimalRaw values,
creating the standard generators over those and then
incorporating the generators for the mantissa.
This approach is useful when the precision has few choices, for example,
the 256 valued Word8 type.  

The standard generators for the DecimalRaw structure will be built
pairing the strategies for both the precision and mantissa values.

Note that in this situation, all of the decimals are considered to be of rank 1,
because only the mantissa is a type variable.

\begin{code}
type DecimalW8 = DecimalRaw Word8

enumDecW8 :: Enumeration (DecimalW8) Label
enumDecW8 = mkEnumeration c s
  where c r | r == 1 = baseCounter enumW8
        c _ | otherwise = 0
        s r i | r == 1 = (\p -> Decimal p A) (baseSelector enumW8 i)
        s _ _ | otherwise = undefined

instance Functor (DecimalW8) where
  fmap f (Decimal p m) = Decimal p (f m)

instance Enumerated (DecimalW8) where
  enumeration = enumDecW8

instance Structure (DecimalW8) where
  substitute (Decimal p _) (x:xs) = (Just (Decimal p x), xs)
  substitute      _           []  = (Nothing, [])

stdGensDecLbl = stdEnumGens :: StandardGens ((DecimalW8) Label)

instance (Show i, EnumGC i) => Testable (DecimalW8 i) where
  stdTestGens = substStdGenStd stdGensDecLbl baseEnumGCStdGens

\end{code}

\begin{code}

\end{code}
\begin{code}

\end{code}
--\begin{code}


