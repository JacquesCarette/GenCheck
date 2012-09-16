{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}

module Decimal_GC where

--import Data.Decimal
import Data.Word
import System.Random (StdGen, split)

import Test.GenCheck
-- import Test.GenCheck.Generator.Enumeration (Enumerated(enumeration), Enumeration, eProd, Label)
import Test.GenCheck.Generator.BaseEnum
import Test.GenCheck.Generator.Generator
import Test.GenCheck.Generator.BaseGens
import Test.GenCheck.Generator.EnumStrat

data DecimalRaw w8 i = Decimal {
      decimalPlaces :: ! w8,
      decimalMantissa :: ! i}
instance (Show w8, Show i) => Show (DecimalRaw w8 i) where
  show (Decimal p m) = (show m) ++ " x10^" ++ (show p)

instance Structure2 DecimalRaw where
  substitute2 = subDec
subDec :: DecimalRaw a b -> [j] -> [i] -> (Maybe (DecimalRaw j i), [j], [i])
subDec (Decimal _ _) (p:ps) (m:ms) = (Just (Decimal p m), ps, ms)
subDec _ ps ms = (Nothing, ps, ms)

-- Make a generator for the Decimal structure, noting there
-- is only one Decimal structure when the holes are unlabelled
genDecStruct :: Generator (DecimalRaw Label Label)
genDecStruct r | r == 2    = [Decimal A B]
genDecStruct _ | otherwise = []

-- Set up the Word8 precision as a base enumeration (should be in BaseEnum)
-- and define the standard generators
enumW8 = enumList ([minBound..maxBound]::[Word8]) 
stdGensW8 = baseEnumGCGens enumW8

-- AssumW that the mantissa type is an EnumGC class so 
-- there is a base enumeration for those standard generators.
-- To use Integral, a subset of the Integers will have to be used
-- and an enumeration of these with a range will do the trick.
enumInteger n = enumList ([(-n), n])
instance EnumGC Integer where
  base = enumInteger 100000

-- the full generators for DecimalRaw are given by substituting
-- with the mantissa generator and the precision generator.
-- Restricting these to the standard strategies leaves 16 options,
-- namely any pair of the 4 strategies for either.
--   Here are 4 of them using the random and boundary (extreme) generators.
-- Note that the strategies are mixed, but that is arbitrary.

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



