A collection of tests to demonstrate the base generators working correctly.

\begin{code}
import System.Random

import Test.GenCheck.Generator.BaseGens as Base
import Test.GenCheck.Generator.Generator

\end{code}

\begin{code}
sk = System.Random.mkStdGen 928398231

dspGen lbl gen = putStrLn $ lbl ++ (show (genTake 1 20 gen))

baseGenTest = 
  do sd <- System.Random.getStdGen
     dspGen "Exhaustive Int" genIntAll
     dspGen "Random Int"  (genIntRnd sd)
     dspGen "Extreme Int"  genIntExt
     dspGen "Uniform 20 Int"  (genIntUni 20)

baseRngGenTest = 
  do sd <- System.Random.newStdGen
     dspGen "Exhaustive [-5,5]"       (genBaseRngAll (-5::Int,5))
     dspGen "Random  [-100,100]"      (genBaseRngRnd (-100::Int,100) sd)
     dspGen "Extreme  [-100,100]"     (genBaseRngExt (-100::Int,100)) 
     dspGen "Extreme  [0,100]"        (genBaseRngExt (0::Int,100)) 
     dspGen "Uniform 20  [-100,100]"  (genBaseRngUni (-100::Int,100) 20)
     dspGen "Uniform 11  [-100,100]"  (genBaseRngUni (-100::Int,100) 11)

\end{code}

\begin{code}


\end{code}

\begin{code}

main = 
 do baseGenTest
    baseRngGenTest
\end{code}
