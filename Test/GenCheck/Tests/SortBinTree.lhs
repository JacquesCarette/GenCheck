The BinTree binary tree with external nodes structure and sorting functions.

sortBinT sorts the elements ascending from left to right, 
while retaining the shape of the tree.

biggest (smallest) returns the largest (smallest) element in the binary tree.

Note that an instance of show has been provided for BinT;
this is a requirement for the SimpleCheck reporting.

\begin{code}
module Test.GenCheck.Tests.SortBinTree where

data BinTree a = BTNode a | BTBr (BinTree a) (BinTree a)
instance Functor BinTree where
  fmap = fmap_bintree

fmap_bintree f (BTNode x) = BTNode (f x)
fmap_bintree f (BTBr bt1 bt2) = BTBr (fmap_bintree f bt1) (fmap_bintree f bt2)

sortBinTree :: (Ord a) => BinTree a -> BinTree a
sortBinTree t@(BTNode _) = t
sortBinTree   (BTBr tl tr) = 
  let tl' = sortBinTree tl
      tr' = sortBinTree tr
      bl = biggest tl'
      sr = smallest tr'
  in if bl <= sr then BTBr tl' tr'
     else sortBinTree $ BTBr (rpl bl sr tl) (rpl sr bl tr)
  where
    rpl x x' tree@(BTNode y)   | x == y    = BTNode x'
                               | otherwise = tree
    rpl x x' (BTBr tl' tr') = BTBr (rpl x x' tl') (rpl x x' tr')

biggest :: (Ord a) => BinTree a -> a
biggest (BTNode x) = x
biggest (BTBr tl tr) = max (biggest tl) (biggest tr)

smallest :: (Ord a) => BinTree a -> a
smallest (BTNode x) = x
smallest (BTBr tl tr) = min (smallest tl) (smallest tr)


instance (Show a) => Show (BinTree a) where
  showsPrec d (BTNode m) = 
      showParen (d > app_prec) $ showsPrec (app_prec+1) m
        where app_prec = 10
  showsPrec d (BTBr u v) = 
      showParen (d > up_prec) $
        showsPrec (up_prec+1) u . showString "/\\" . showsPrec (up_prec+1) v
          where up_prec = 5
\end{code}

