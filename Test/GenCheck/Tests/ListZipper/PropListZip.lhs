A small set of properties from the ListZipper package specification.

\begin{code}
module Test.GenCheck.Tests.ListZipper.PropListZip where

import Test.GenCheck (Property)
import Test.GenCheck.Tests.ListZipper.ListZipper as Zipper

\end{code}

A ziplist is empty iff the cursor is both at the beginning and the end

\begin{code}
propEmptyP :: Property (Zipper a)
propEmptyP z = emptyp z == beginp z && endp z
\end{code}

Folding an operation over the zipper with the cursor at the start
should be identical to folding it over the unzipped list. 
Note that the zipper fold folds the cursor through the zipper, 
but always passes the zipper to the function.
(cursor safety is supposed to be guaranteed by the foldz' functions)

\begin{code}

propFoldrz :: (Eq b) => (a -> b -> b) -> b -> Property (Zipper a)
propFoldrz  f y0 zxs = 
   let xs = Zipper.toList zxs 
   in (foldr f y0 xs == foldrz f' y0 (start zxs))
   where f' zs y = f (cursor zs) y

propFoldlz :: (Eq b) => (b -> a -> b) -> b -> Property [a]
propFoldlz  f y0 xs = 
   let zxs = Zipper.fromList xs 
       fval = foldl f y0 xs
       zxs' = start zxs
   in (fval == foldlz f' y0 zxs') && (fval == foldlz' f' y0 zxs')
   where f' y zs = f y (cursor zs)

\end{code}

Replace is equivalent to deleting the current element and inserting a new one.

\begin{code}
propInsDelRepl :: (Eq a) => Property (a, Zipper a)
propInsDelRepl (x, xs) = Zipper.insert x (Zipper.delete xs) == Zipper.replace x xs
\end{code}

\begin{code}
\end{code}


