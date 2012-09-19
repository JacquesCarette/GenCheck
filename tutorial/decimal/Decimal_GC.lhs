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

instance (Integral w8, Integral i, Show w8, Show i) => Show (DecimalRaw w8 i) where
   showsPrec _ (Decimal e n)
       | e == 0     = (concat [signStr, strN] ++)
       | otherwise  = (concat [signStr, intPart, ".", fracPart] ++)
       where
         strN = show $ abs n
         signStr = if n < 0 then "-" else ""
         len = length strN
         padded = replicate (fromIntegral e + 1 - len) '0' ++ strN
         (intPart, fracPart) = splitAt (max 1 (len - fromIntegral e)) padded

{-
instance (Show w8, Show i) => Show (DecimalRaw w8 i) where
  show (Decimal p m) = (show m) ++ " x10^" ++ (show p)
-}
instance Structure2 DecimalRaw where
  substitute2 = subDec
subDec :: DecimalRaw a b -> [j] -> [i] -> (Maybe (DecimalRaw j i), [j], [i])
subDec (Decimal _ _) (p:ps) (m:ms) = (Just (Decimal p m), ps, ms)
subDec _ ps ms = (Nothing, ps, ms)

\end{code}

There is only one possible Decimal structure: the labelled product | Decimal \_ \_ |.
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
Integer mantissa's can also be used - an arbitrary range of 
double the min / max bounds of the Ints is used to set the bounds on the enumeration
(see Test.GenCheck.Generator.BaseEnum for details).

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
genDecXtrmXtrm ::  (EnumGC i) => Generator (DecimalRaw Word8 i)
genDecXtrmXtrm  = subst2N 10 genDecStruct gW8x gix
   where gix    = genXtrm baseEnumGCStdGens
         gW8x   = genXtrm stdGensW8

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

instance (Integral i, Show i, EnumGC i) => Testable (DecimalW8 i) where
  stdTestGens = substStdGenStd stdGensDecLbl baseEnumGCStdGens

\end{code}

Comparing the results of the two approaches clarifies
when a two sort substitution is more or less appropriate
than first enumerating the first sort, then the second.

For example, the extreme mantissa / extreme precision generator
will be useful for testing Decimal arithmetic functions as 
errors are most likely in extreme values.

Note that the arbitrary choice of the range of Integers to test comes up here.

\begin{verbatim}
*Decimal_GC> take 100 $ genDecXtrmXtrm 2 :: [DecimalRaw Word8 Integer]
[-4294967297,4294967293,-4294967296,4294967292,-4294967295,4294967291,
-4294967294,4294967290,-4294967293,4294967289,-0.0000000...0004294967292,
0.000000000000...0000004294967288,-0.000...0004294967291,

...

-429496728.7,429496728.3,-429496728.6,429496728.2,-429496728.5,
429496728.1,-429496728.4,429496728.0,-429496728.3,429496727.9,
-0.000...000004294967282, 0.000...000004294967278,

...
\end{verbatim}

The values are grouped in 10's because of the arbitrary choice made
in the subst2N function.  The first 10 have precision 0, the second 10
precision 255, the third 10 precision 1, fourth 10 precision 254, etc. 
(some of the high precision values  been ellided for legibility).  

The random mantissa / extreme precision generator might be used
to supplement testing to ensure that there are no dependencies on extreme
integer values.

\begin{verbatim}
*Decimal_GC> take 100 $ genDecXtrmRnd (mkStdGen 20398423) 2 :: [DecimalRaw Word8 Integer]
3297478285,-2103007409,-1570044525,3853764760,-553397225,4078554636,
-994370392,-3197401073,-2259414665,-4098040193,-0.000...0003663556986,
-0.000...00000000004161385473,0.000000...00002455986108,

...

292414849.3,57118104.6,293910192.6,-173351886.7,-351335950.1,-242506370.5,
-100176922.4,333869685.3,-1741099.5,359802003.0,0.0000...002669858400,
0.00000000...000003178902858,

...
\end{verbatim}

For completeness, there should be some tests on intermediate precision values,
such as provided by the random mantissa / random precision generator.
\begin{verbatim}
*Decimal_GC> take 40 $ genDecRndRnd (mkStdGen 873486) (mkStdGen 20398423) 2 :: [DecimalRaw Word8 Integer]
[0.0000... 000003297478285,-0.0000...00002103007409,-0.0000...00001570044525,
0.00000...0000003853764760,
...
\end{verbatim}

It is worth noting that this test suite is quite unlike that which
would be prepared by a human test writer, which is why
the automatic testing is so well suited to complement manual test cases.

