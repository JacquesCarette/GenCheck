\begin{code}
module Test_ListZipper where

import System.Random(mkStdGen)

import Test.GenCheck.System.SimpleCheck 
           (stdTest, stdTestArgs, stdReport, stdReportArgs)

-- because we're doing more than just the usual stuff
import Test.GenCheck 
import Test.GenCheck.Generator.BaseGens (genIntAll, genLowCharAll, genUpperCharRnd)

-- the specification, module, and GenCheck instances for the Zipper data type
import PropListZip
import ListZipper as Zip
import ListZipper_GC()

genZip_Char :: StandardGens (Zipper Char)
genZip_Char = substStdGenAll (stdTestGens :: StandardGens (Zipper Label))  
                             genLowCharAll

genValZip_Char :: Rank -> [(Char, Zipper Char)]
genValZip_Char r = 
  let gc = (genUpperCharRnd (mkStdGen 657985498)) -- random value generator
      gz = genAll genZip_Char -- exhaustive zipper generator
  in zip (gc 1) (gz r) -- (Char, Zipper Char) generator with random Char, exhaustive zippers

testFoldrz_1, testFoldrz_2 :: Rank -> Count -> IO (r)
testFoldrz_1 r n = 
  let propFoldrz_str = propFoldrz (:) "" :: Property (Zipper Char)
      lbl = "Compare concatenating strings with right fold, up to rank " ++ (show r)
  in  stdTestArgs genZip_Char lbl r propFoldrz_str n
testFoldrz_2 r n = 
  let propFoldrz_str = propFoldrz (:) "" :: Property (Zipper Char)
      lbl = "Compare concatenating strings with right fold, up to rank " ++ (show r)
  in  stdReportArgs genZip_Char lbl r propFoldrz_str n

testFoldlz_1, testFoldlz_2 :: Rank -> Count -> IO (r)
testFoldlz_1 r n = 
  let propFoldlz_sum = propFoldlz (+) 0 :: Property [Int]
      lbl = "Compare sum over integer list to left fold, up to rank " ++ (show r)
  in  stdTest propFoldlz_sum n
testFoldlz_2 r n = 
  let propFoldlz_sum = propFoldlz (+) 0 :: Property [Int]
      lbl = "Compare sum over integer list to left fold, up to rank " ++ (show r)
  in  stdReport propFoldlz_sum n

testInsDelRepl_1 :: Rank -> IO (r)
testInsDelRepl_1 r = 
  let lbl = "Compare insert/delete to replace, up to rank " ++ (show r)
      -- do up to maxr tests for zippers up to rank maxr
      instruct maxr = [(rk, toInteger maxr) | rk <- [1..maxr]] 
      suite = genSuite genValZip_Char (instruct r)
  in  simpleReport lbl propInsDelRepl suite
\end{code}
