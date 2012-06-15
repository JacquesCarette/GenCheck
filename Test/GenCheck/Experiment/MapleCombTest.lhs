
MaplePrint converts a data value into a Maple representation of the value

\begin{code}

module Test.GenCheck.Generator.MapleCombTest where

import Test.GenCheck.Generator.Descriptor
import Test.GenCheck.System.GenCheck(Rank,Count)

class MaplePrint c where
  mapleValue :: (Show a) => c a -> String
  mapleType  :: (Show a) => c a -> String
  prettyMaple :: (Show a) => c a -> String

newtype MapleType c = Mpl (c -> String)  -- c must be of the Show class
unMpl (Mpl x) = x

instance Descriptor MapleType where
  const   x = Mpl $ show x
  node  c x = 
  sum   x y = 
  prod2 c x y = 
  prod3 c x y z = 

\end{code}

