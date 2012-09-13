\begin{code}
module Test.GenCheck.Tests.ListZipper.TestListZipper where

import System.Random(mkStdGen, StdGen)

import Test.GenCheck 
import Test.GenCheck.Generator.BaseGens (genIntAll, genLowCharAll, genUpperCharRnd)
import Test.SimpleCheck (stdTest, stdTestArgs, stdReport, stdReportArgs, simpleReport, SimpleResults)

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

testFoldrz :: Rank -> Count -> IO (SimpleResults (Zipper Char))
testFoldrz r n = 
  let propFoldrz_str = propFoldrz (:) "" :: Property (Zipper Char)
      lbl = "Compare concatenating strings with right fold, up to rank " ++ (show r)
  in  stdTestArgs genZip_Char lbl r propFoldrz_str n

testFoldlz :: Rank -> Count -> IO (SimpleResults [Int])
testFoldlz r n = 
  let propFoldlz_sum = propFoldlz (+) 0 :: Property [Int]
      lbl = "Compare sum over integer list to left fold, up to rank " ++ (show r)
  in  stdTest propFoldlz_sum n

testInsDelRepl :: Rank -> IO (SimpleResults (Char, Zipper Char))
testInsDelRepl r = 
  let lbl = "Compare insert/delete to replace, up to rank " ++ (show r)
      -- do up to maxr tests for zippers up to rank maxr
      instruct maxr = [(rk, toInteger maxr) | rk <- [1..maxr]] 
      suite = genSuite genValZip_Char (instruct r)
  in  simpleReport lbl propInsDelRepl suite
\end{code}
