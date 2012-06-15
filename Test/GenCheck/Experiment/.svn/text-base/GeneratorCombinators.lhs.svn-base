These used to be in Test.GenCheck.Generator.Generator but were never used.
Moved here for now.

\begin{code}
import Prelude hiding (zipWith, zipWith3)
import Data.List (zipWith, zipWith3, zipWith4, zipWith5, genericTake)
\end{code}

Generator combinators construct new generators:

genFilter:     only include elements satisfying a boolean property 
rankTransform: transform the rank schema of a generator
genCycle :     converts a finite generator into an infinite generator

\begin{code}
genMap :: (a->b) -> Generator a -> Generator b
genMap f g = \r -> map f (g r)

genZipWith :: (a->b->c) -> Generator a -> Generator b -> Generator c
genZipWith c ga gb r = zipWith c (ga r) (gb r)
genZip :: Generator a -> Generator b -> Generator (a,b)
genZip = genZipWith (,)

genZipWith3 :: (a->b->c->d) -> Generator a -> Generator b -> Generator c -> Generator d
genZipWith3 c ga gb gc r = zipWith3 c (ga r) (gb r) (gc r)
genZip3 :: Generator a -> Generator b -> Generator c -> Generator (a,b,c)
genZip3 = genZipWith3 (,,)

genZipWith4 :: (a->b->c->d->e) -> Generator a -> Generator b -> Generator c -> Generator d -> Generator e
genZipWith4 c ga gb gc gd r = zipWith4 c (ga r) (gb r) (gc r) (gd r)
genZip4 :: Generator a -> Generator b -> Generator c -> Generator d -> Generator (a,b,c,d)
genZip4 = genZipWith4 (,,,)

genZipWith5 :: (a->b->c->d->e->f) -> Generator a -> Generator b -> Generator c -> Generator d -> Generator e -> Generator f
genZipWith5 c ga gb gc gd ge r = zipWith5 c (ga r) (gb r) (gc r) (gd r) (ge r)
genZip5 :: Generator a -> Generator b -> Generator c -> Generator d -> Generator e -> Generator (a,b,c,d,e)
genZip5 = genZipWith5 (,,,,)

genFilter :: (a -> Bool) -> Generator a -> Generator a
genFilter p g = \r -> filter p (g r)

rankTransform ::Generator a ->  (Rank -> Rank) -> Generator a
rankTransform g rtfm = \r -> g (rtfm r)

genRepeat :: Generator a -> Generator a
genRepeat g r = cycle (g r)

\end{code}
