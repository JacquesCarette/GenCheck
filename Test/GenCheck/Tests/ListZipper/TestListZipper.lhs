\begin{code}
module Test.GenCheck.Tests.ListZipper.TestListZipper where

import System.Random(mkStdGen, StdGen)

import Test.GenCheck 
import Test.GenCheck.Generator.BaseGens (genIntAll, genLowCharAll, genUpperCharRnd)
import Test.GenCheck.Tests.PureTest

-- the specification, module, and GenCheck instances for the Zipper data type
import Test.GenCheck.Tests.ListZipper.PropListZip
import Test.GenCheck.Tests.ListZipper.ListZipper as Zip
import Test.GenCheck.Tests.ListZipper.ListZipper_GC()

genZip_Char :: StandardGens (Zipper Char)
genZip_Char = substStdGenAll (stdTestGens :: StandardGens (Zipper Label))  
                             genLowCharAll

genValZip_Char :: Rank -> [(Char, Zipper Char)]
genValZip_Char r = 
  let gc = (genUpperCharRnd (mkStdGen 657985498)) -- random value generator
      gz = genAll genZip_Char -- exhaustive zipper generator
  in zip (gc 1) (gz r) -- (Char, Zipper Char) generator with random Char, exhaustive zippers

-- Compare concatenating strings with right fold, up to rank r
testFoldrz :: StdGen -> Rank -> Count -> GCResults (Zipper Char)
testFoldrz s r n = 
  let propFoldrz_str = propFoldrz (:) "" :: Property (Zipper Char)
  in  gcPureTest propFoldrz_str $ stdSuite genZip_Char s r n

-- Compare sum over integer list to left fold, up to rank r
-- use the default standard generators and standard suite through gcPureTestable
testFoldlz :: StdGen -> Rank -> Count  -> GCResults [Int]
testFoldlz s r n = 
  let propFoldlz_sum = propFoldlz (+) 0 :: Property [Int]
  in  gcPureTestable propFoldlz_sum s r n

-- do up to maxr tests for zippers up to rank maxr
testInsDelRepl :: Rank  -> GCResults (Char, Zipper Char)
testInsDelRepl r = 
  let instruct maxr = [(rk, toInteger maxr) | rk <- [1..maxr]] 
      suite = genSuite genValZip_Char (instruct r)
  in  gcPureTest propInsDelRepl suite
\end{code}
