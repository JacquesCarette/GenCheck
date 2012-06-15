\subsubsection{SmallCheck Compatibility}

The Serial class requires an exhaustive generator for the type.
Composition of serial classes produces a serial class
through substitution of all combinations of the underlying set.

Note that this creates an enormous number of test cases
even for very small structures and base sets,
so this should be used with caution.

\begin{code}
module SC_Compabitility where

class Serial a where
  series :: Generator a 

\end{code}

These aren't good product generators, they won't work for non-finite generators,
but have been included as they match the SmallCheck product generators.

\begin{code}
genProd :: Generator a -> Generator b -> Generator (a,b)
genProd gx gy = \r ->
  let rcs = compositions1 2 r
  in concatMap pg rcs where
     pg [r1,r2] = [(x,y) | x <- gx r1, y <- gy r2]

genProd3 :: Generator a -> Generator b -> Generator c -> Generator (a,b,c)
genProd3 gx gy gz = \r -> 
  let rcs = compositions1 3 r
  in concatMap pg rcs where
     pg [r1,r2,r3] = [(x,y,z) | x <- gx r1, y <- gy r2, z <- gz r3]

genProd4 :: Generator a -> Generator b -> Generator c -> Generator d -> Generator (a,b,c,d)
genProd4 gx gy gz gw = \r -> 
  let rcs = compositions1 4 r
  in concatMap pg rcs where
     pg [r1,r2,r3,r4] = [(x,y,z,w) | x <- gx r1, y <- gy r2, 
                                     z <- gz r3, w <- gw r4]

\end{code}
