\subsection{QuickCheck Compatibility}

The Arbitrary class for QuickCheck is a
random value generator,
based on the size of the structure.
Composition is with sets of random values,
from other Arbitrary class random generators.

\begin{code}
module QC_Compatibility where

import Generator
-- import Generators
-- import SuiteBuild
-- import Results
-- import GenCheck

(==>) = filterG

resize r' g = \_ -> g r'
two   x = prodG x x
three x = prodG3 x x x
four  x = prodG4 x x x x

\end{code}

