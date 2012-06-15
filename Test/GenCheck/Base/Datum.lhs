The Datum class describes a record that has a significant value (the datum)
and possibly additional information (metadata) about that value.
The purpose of the class is to allow components of a system to 
pass metadata through other components which are only aware of the datum type.

The Identity functor from the Transformers package is the Datum instance
where the test requires no additional information or meta-data.

\begin{code}
{-# LANGUAGE TypeFamilies #-}

module Test.GenCheck.Base.Datum (Datum(..)) where

import Data.Functor.Identity    

class Datum t where
  type DataType t :: *
  datum :: t -> DataType t

instance Datum (Identity a) where
  type DataType (Identity a) = a
  datum = runIdentity
\end{code}
