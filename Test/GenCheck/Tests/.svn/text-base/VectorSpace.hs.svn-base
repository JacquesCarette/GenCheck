module VectorSpace where 

import Math.Algebra.Field.Base
import Math.Algebras.VectorSpace
import Test.QuickCheck

prop_AddGrp (x,y,z) =
    x <+> (y <+> z) == (x <+> y) <+> z && -- associativity
    x <+> y == y <+> x                 && -- commutativity
    x <+> zero == x                    && -- identity
    x <+> neg x == zero                   -- inverse

prop_VecSp (a,b,x,y,z) =
    prop_AddGrp (x,y,z) &&
    a *> (x <+> y) == a *> x <+> a *> y && -- distributivity through vectors
    (a+b) *> x == a *> x <+> b *> x     && -- distributivity through scalars
    (a*b) *> x == a *> (b *> x)         && -- associativity
    1 *> x == x                            -- unit

prop_VecSpQn (a,b,x,y,z) = prop_VecSp (a,b,x,y,z)
    where types = (a,b,x,y,z) :: (Q, Q, Vect Q EBasis, Vect Q EBasis, Vect Q EBasis)


-- QuickCheck generators

instance Arbitrary EBasis where
    arbitrary = do n <- arbitrary :: Gen Int
                   return (E n)

instance Arbitrary Q where
    arbitrary = do n <- arbitrary :: Gen Integer
                   d <- arbitrary :: Gen Integer
                   return (if d == 0 then fromInteger n else fromInteger n / fromInteger d)

instance Arbitrary (Vect Q EBasis) where
    arbitrary = do ts <- arbitrary :: Gen [(EBasis, Q)]
                   return $ nf $ V ts

-- SimpleCheck generator: Exhaustive

main = 
  do  putStr "QuickCheck tests: "
      quickCheck prop_VecSpQn
      putStrLn
      putStrLn "SimpleCheck tests"

