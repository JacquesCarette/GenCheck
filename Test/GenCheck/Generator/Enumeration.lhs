Enumerations of a structured type (aka type constructor) provides the number of
shapes as well as a strict total ordering over the shapes of that (structured)
type.  One approach to generating instances of an enumerated type is to
generate indices into that enumeration to ``pick'' values of the type (when it
does not lead to too much confusion, we will refer to a particular shape of a
structured type (i.e. a tree with holes where data will be filled-in) as a
``value'').  This separation allows the generation of indices - the testing
``strategy'' - to be independent of the construction of concrete instances of
the type.  Enumerative generators use a generic strategy to sample value from
an enumerated type.

In the GenCheck package, the indexing scheme for enumerations has been fixed to
a pair consisting of a Rank (Int) and a Count (Integer), both of which are
natural numbers (i.e. positive integers).  The most general definition of rank
is a positive natural number that indexes (finite but possibly empty)
partitions of the set of elements of the type.  This two level indexing schema
is recognized by all of the components of GenCheck and is generally the default
mechanism for generating and reporting values.

Two flavours of enumerations are provided in the GenCheck package:

\begin{enumeration}
\item Haskell base (or scalar) types 
\item Haskell regular polynomial types (i.e. non-nested Haskell types)
\end{enumeration}

The rank of all base types such as |Int| and |Char| is 1, and the rank of
regular polynomial data types is the number of holes for data elements in the
structure The second index (Count) is an arbitrary Integer type to allow very
large enumerations.

The built in enumeration (Enum) is used for Haskell base types that are
instances of Haskell's Bounded and Enumerated classes (e.g. |[minBound ..
maxBound]|).  Enumerations are also provided for |Integer|, |Ratio|,
|Rational|, |Float| and |Double|.  These ``scalar'' types require some
additional structure.  Most of the base type enumeration functions are in
BaseTypeEnum.lhs.

All Haskell regular polynomial structure types can be enumerated by rank (size)
using a mechanical algorithm based solely on the type constructor.  These
enumerations are constructed using the combinators eConst, eNode, eSum, eProd,
etc.  to mirror the type's constructor.  Recursive structure enumerations
should also be memoized to improve their performance since the count /
selection of value at rank r is dependent on the count / selection of values at
ranks $1$ through $(r-1)$. The eMemoize function provides the default
memoization; additional memoization techniques are also provided in Memoize.lhs

Note: the Enumeration class differs from Haskell's Enum class in that
the index of the enumerated value is unrecoverable, so the methods
succ, pred, enumToInt, etc. are not required for the Enumeration instances.

\begin{code}

module Test.GenCheck.Generator.Enumeration
( Enumeration
, Enumerated(..)
, Counter
, Selector
, mkEnumeration
, mkEnum
, counter
, selector
, get
, getUnsafe
, enumRange
, eMemoize
, eConst
, eNode
, eSum
, eSum3
, eSum4
, eProd
, eProd3
, eProd4
, cConst
, cNode
, cSum
, cSum3
, cSum4
, cProd
, cProd3
, cProd4
, sConst
, sNode
, sSum
, sSum3
, sSum4
, sProd
, sProd3
, sProd4
, Label(..)
) where 

import Math.Combinat.Combinations (combinations1)
import Data.Function.Memoize

import Test.GenCheck.Base.Base(Rank,Count)
\end{code}

Note that the actual implementation of |Enumeration| is not exported.  
Instead, smart constructors |mkEnumeration| and |mkEnum| are provided.
|mkEnumeration| applies a default memoization strategy to both the counter and
selector functions.  |mkEnum| is just the raw constructor, and is used for
enumerations that will be embedded in other enumerations to avoid redundant
memoization.  |mkEnum| should only be needed by experts and the Template Haskell
code that assembles the enumeration combinators based on the type constructor.

An enumerated type may be defined to be an instance of the |Enumerated| class,
which will automatically provide the default generators for that type
as instances of StandardGens in the Generator module.
The structures will have a Label (A, B, etc.) in each ``hole''
that distinguishes the sort of the element.

|get| retrieves a value from an enumeration given a rank and an index, 
or returns |Nothing| if the index is outside of the range of the enumeration.
|getUnsafe| assumes the rank and index values are valid and in range,
with unpredictable results if not.

\begin{code}
type Counter      = Rank -> Count
type Selector c a = Rank -> Count -> c a

data Enumeration c a = Enum Counter (Selector c a)
data Label = A | B | C | D | E | F | G deriving Show

instance Functor c => Functor (Enumeration c) where
    fmap f (Enum c s) = Enum c (\rank count -> fmap f (s rank count))

mkEnum, mkEnumeration :: Counter -> Selector c a -> Enumeration c a
mkEnum = Enum
mkEnumeration c s = eMemoize $ mkEnum c s

counter :: Enumeration c a -> Counter
counter (Enum c _) = c
selector :: Enumeration c a -> Selector c a
selector (Enum _ s) = s

class Functor c => Enumerated c where
  enumeration :: Enumeration c Label
  enumFromTo :: (Integer,Integer) -> Enumeration c Label
  enumFromTo (l,u ) = enumRange (l,u) enumeration

enumRange :: (Count, Count) -> Enumeration c a -> Enumeration c a
enumRange (l,u) e = mkEnum c' s' 
  where c = counter e
        s = selector e
        c' r = min (c r) (u - l + 1)
        s' r k' = s r (k' + l - 1)

get :: Enumeration c a -> Rank -> Count -> Maybe (c a)
get (Enum c s) r n | (r > 0) && (n > 0) =  
    if (c r) > n then Just (s r n) else Nothing
get _ _ _ | otherwise = Nothing

getUnsafe :: Enumeration c a -> Rank -> Count -> c a
getUnsafe  (Enum _ s) r n = s r n
\end{code}

Enumerations are much more efficient when memoized. The counting and selector
functions built by these combinators are not memoized to avoid redundant
memoization.  It is appropriate to memoize the resulting enumeration using
eMemoize, which selects the current default memoization technique.
Other memoize techniques can be located in the Base.Memoize module.

\begin{code}
eMemoize :: Enumeration c a -> Enumeration c a
eMemoize (Enum c s) =  mkEnum (memoize c) (memoize s)
\end{code}

Enumerations can be mechanically constructed for regular polynomial types.
eConst, eNode, eSum, eProd, etc. are combinators for constructing enumerations.
They rely on counting (cConst, etc.) and selector (sConst, etc.) combinators.
The pattern matching on the sums must be lazy (irrefutable) to allow the
manual counting of the possible structures of that size to terminate the
recursive construction of the patterns.

\begin{code}
eConst, eNode :: c a -> Enumeration c a
eConst x = mkEnum cConst (sConst x)
eNode  x = mkEnum cNode  (sNode  x)

eSum :: Enumeration c a -> Enumeration c a -> Enumeration c a
eSum  e1@(~(Enum c1 _)) e2@(~(Enum c2 _)) = mkEnum (cSum c1 c2) (sSum e1 e2)
eSum3 :: Enumeration c a -> Enumeration c a -> Enumeration c a -> Enumeration c a
eSum3 e1@(~(Enum c1 _)) e2@(~(Enum c2 _)) e3@(~(Enum c3 _)) 
          = mkEnum (cSum3 c1 c2 c3) (sSum3 e1 e2 e3)
eSum4 :: Enumeration c a -> Enumeration c a -> Enumeration c a -> Enumeration c a -> Enumeration c a
eSum4 e1@(~(Enum c1 _)) e2@(~(Enum c2 _)) e3@(~(Enum c3 _)) e4@(~(Enum c4 _))
          = mkEnum (cSum4 c1 c2 c3 c4) (sSum4 e1 e2 e3 e4)

eProd :: (a x -> b x -> c x) -> Enumeration a x -> Enumeration b x -> Enumeration c x
eProd con e1@(Enum c1 _) e2@(Enum c2 _) = mkEnum (cProd c1 c2) (sProd con e1 e2)
eProd3 :: (a x -> b x -> c x -> d x) -> Enumeration a x -> Enumeration b x -> Enumeration c x -> Enumeration d x
eProd3 con e1@(Enum c1 _) e2@(Enum c2 _) e3@(Enum c3 _) = 
         mkEnum (cProd3 c1 c2 c3) (sProd3 con e1 e2 e3)
eProd4 :: (a x -> b x -> c x -> d x -> e x) -> Enumeration a x -> 
        Enumeration b x -> Enumeration c x -> Enumeration d x -> Enumeration e x
eProd4 con e1@(Enum c1 _) e2@(Enum c2 _) e3@(Enum c3 _) e4@(Enum c4 _) = 
         mkEnum (cProd4 c1 c2 c3 c4) (sProd4 con e1 e2 e3 e4)
\end{code}

Counting algebraic data types. The enumProd(3) functions compute the number of
instances for all possible distributions of the total rank within a product
constructor; they are defined below.

\begin{code}
cConst, cNode :: Counter
cConst r = if r == 1 then (1::Count) else (0::Count)
cNode  r = if r == 1 then (1::Count) else (0::Count)
cSum, cProd :: Counter -> Counter -> Counter
cSum c1 c2 r = c1 r + c2 r
cProd c1 c2 r = sum $ map product (sizes r)
    where sizes r' = map (zipWith ($) [c1, c2]) (combinations1 2 r')
cSum3, cProd3 :: Counter -> Counter -> Counter -> Counter
cSum3 c1 c2 c3 r = c1 r + c2 r + c3 r
cProd3 c1 c2 c3 r = sum $ map product (sizes r)
    where sizes r' = map (zipWith ($) [c1, c2, c3]) (combinations1 3 r')
cSum4, cProd4 :: Counter -> Counter -> Counter -> Counter -> Counter
cSum4 c1 c2 c3 c4 r = c1 r + c2 r + c3 r + c4 r
cProd4 c1 c2 c3 c4 r = sum $ map product (sizes r)
    where sizes r' = map (zipWith ($) [c1, c2, c3, c4]) (combinations1 4 r')

\end{code}

Selector functions for algebraic data types, builds a map from
the rank and index count to a value of the type.

sConst,sNode,sSum, sProd, etc. mirror the type constructor algebra.
sSum3,sSum4,sProd3,sProd4 could have been excluded, 
with larger products and sums being built pairwise,
but incorporating them in a single function allows
all of the dimensions to be treated equally and avoids 
creating and destroying intermediate tuples.

The interval and deref functions ``find'' the appropriate value
in the enumeration through some rather ugly arithmetic,
and are defined below.

\begin{code}
sConst,sNode :: (Num a, Num b, Eq a, Eq b) => t x -> a -> b -> t x
sConst x r n = if (r==1) && (n==1) then x else selector_error
sNode  x r n = if (r==1) && (n==1) then x else selector_error
sSum :: Enumeration c a -> Enumeration c a -> Selector c a
sSum (Enum c1 s1) (Enum c2 s2) r n = 
  let n1 = c1 r in if (n <= n1) then s1 r n 
                   else if (n <= c2 r) then s2 r (n - n1)
                   else selector_error

sSum3 :: Enumeration c a -> Enumeration c a -> Enumeration c a -> Selector c a
sSum3 (Enum c1 s1) (Enum c2 s2) (Enum c3 s3) r n = 
  let n1 = c1 r 
      n2 = c2 r
      in if          (n <= n1)   then s1 r n 
           else   if (n <= n2)   then s2 r (n - n1)
           else   if (n <= c3 r) then s3 r (n - (n1+n2))
           else   selector_error

sSum4 :: Enumeration c a -> Enumeration c a -> Enumeration c a -> Enumeration c a -> Selector c a
sSum4 (Enum c1 s1) (Enum c2 s2) (Enum c3 s3) (Enum c4 s4) r n = 
  let n1 = c1 r 
      n2 = c2 r
      n3 = c3 r
      in if          (n <= n1)   then s1 r n 
           else   if (n <= n2)   then s2 r (n - n1)
           else   if (n <= n3)   then s3 r (n - (n1+n2))
           else   if (n <= c4 r) then s4 r (n - (n1+n2+n3))
           else   selector_error

sProd ::  (a x -> b x -> c x) -> Enumeration a x -> Enumeration b x ->Selector c x
sProd con (Enum c1 s1) (Enum c2 s2) r n =
   let ([r1,r2],n') = interval n $ enumProd c1 c2 r
       [n1,n2]      = deref [c1 r1, c2 r2] n'
   in con (s1 r1 n1) (s2 r2 n2)

sProd3 ::  (a x -> b x -> c x -> d x) -> Enumeration a x -> Enumeration b x -> Enumeration c x -> Selector d x
sProd3 con (Enum c1 s1) (Enum c2 s2)  (Enum c3 s3) r n =
   let ([r1,r2,r3],n') = interval n $ enumProd3 c1 c2 c3 r
       [n1,n2,n3]      = deref [c1 r1, c2 r2, c3 r3] n'
   in con (s1 r1 n1) (s2 r2 n2) (s3 r3 n3)

sProd4 ::  (a x -> b x -> c x -> d x -> e x) -> Enumeration a x -> 
    Enumeration b x -> Enumeration c x -> Enumeration d x -> Selector e x
sProd4 con (Enum c1 s1) (Enum c2 s2)  (Enum c3 s3) (Enum c4 s4) r n =
   let ([r1,r2,r3,r4],n') = interval n $ enumProd4 c1 c2 c3 c4 r
       [n1,n2,n3,n4]      = deref [c1 r1, c2 r2, c3 r3, c4 r4] n'
   in con (s1 r1 n1) (s2 r2 n2) (s3 r3 n3) (s4 r4 n4)

selector_error :: t
selector_error = error "Selector error"
\end{code}

The enumeration of products requires somewhat more complicated indexing.
A product of rank r is a pair $(\alpha_{r1}, \beta_{r2})$,
where $r1 + r2 = r$ are a partition of the rank,
so the set of products is given by all products over
all possible combinations of $(r1,r2)$.
For an n-product, this is an n-tuple,
with the argument ranks summing to $r$.
When enumerating an n-product, it is necessary to enumerate
all possible partitions of the rank into n components,
and then enumerate each of the rank decompositions.

enumProd, enumProd3, enumProd4 count the rank decomposition instances of 
a product of other structures given their counting functions.
The count is given for each rank decomposition separately in a list,
the sum being the total number of structures of the product.
The output is a function from rank to a list of the number of 
decompositions of the rank, with a minimum rank of 1, of pairs consisting of
the count and what is actually a pair (or triple) of ranks.

Combinations1 comes from the Math.Combinat package,
and returns a list of length n lists of the partition;
e.g. |combinations1 2 5 = [[1,4],[2,3],[3,2],[4,1]]|.
Note that the rank of all structures must be at least 1.

\begin{code}
-- Rank -> [(Count, [r1,r2])] where r1 + r2 = Rank
enumProd :: (Rank -> Count) -> (Rank -> Count) -> (Rank -> [(Count,[Rank])])
enumProd c1 c2 r = zip (map product sizes) combs
    where combs = combinations1 2 r
          sizes = map (zipWith ($) [c1, c2]) combs

-- Rank -> [(Count, [r1,r2,r3])] where r1 + r2 + r3 = Rank
enumProd3 :: (Rank -> Count) -> (Rank -> Count) -> (Rank -> Count) 
               -> (Rank -> [(Count,[Rank])])
enumProd3 c1 c2 c3 r = zip (map product sizes) combs
    where combs = combinations1 3 r
          sizes = map (zipWith ($) [c1, c2, c3]) combs

-- Rank -> [(Count, [r1,r2,r3,r4])] where r1 + r2 + r3 + r4 = Rank
enumProd4 :: (Rank -> Count) -> (Rank -> Count) -> (Rank -> Count) -> (Rank -> Count) 
               -> (Rank -> [(Count,[Rank])])
enumProd4 c1 c2 c3 c4 r = zip (map product sizes) combs
    where combs = combinations1 4 r
          sizes = map (zipWith ($) [c1, c2, c3, c4]) combs

\end{code}

The Enumeration indexes n-products as a n-dimensional cubes.
\emph{interval} takes an index into a rank r n-product and determines the 
ranks $(r1, ..., rn)$ of each of the n components of the n-product where 
$\sum r_{i} = r$, and an index into just the n-products of that particular rank
distribution.

\emph{deref} converts the one dimensional index into the n-product,
along with the ranks of each of the components of the product,
into an n-tuple index into each component.

\begin{code}
interval :: (Integral k) => k -> [(k,a)] -> (a, k)
interval k' ps = intv k' 0 ps where
  intv _ _ [] = error "Trying to partition an empty interval"
  intv k _ _ | k < 1 = error "Trying to partition a one element interval"
  intv k acc ((p, x) : ps') = 
     let acc' = acc + p 
     in if (k <= acc') then (x, (k-acc)) else intv k acc' ps'

deref :: (Integral k) => [k] -> k -> [k]
deref ds k'  | k' >= 1 =  drf (dimd ds) k'
             | otherwise  = error "Dereferencing below 1"
  where
    drf (dmax:[1]) k | k <= dmax = [k]
    drf (dmax:(dm:dims)) k | k <= dmax =
      let (c,j) = ((k-1) `divMod` dm)
          ds' = drf (dm:dims) (j+1)
      in (c+1) : ds'
    drf _ _ = error "Some weird illegal dereference"
    dimd [] = [1]
    dimd (d:ds') = let (md:mds) = dimd ds' in (md*d : (md:mds))
\end{code}
