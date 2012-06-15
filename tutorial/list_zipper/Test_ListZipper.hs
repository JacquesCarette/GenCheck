module Test_ListZipper where

import System.Random(StdGen, mkStdGen)

import Test.GenCheck 

import Test.GenCheck.Generator.Generator (Generator, StandardGens(..), 
              Label(..), genRepeat, stdEnumGens, genZip)
import Test.GenCheck.Generator.BaseGens
import Test.GenCheck.Generator.StructureGens
import Test.GenCheck.Generator.Substitution (substStdGenAll)
import Test.GenCheck.System.TestSuite (genSuite)
import Test.GenCheck.System.SimpleCheck (stdTest, stdTestArgs, simpleTest)

import ListZipper as Zip
import ListZipper_GC
import PropListZip

genZip_Char = substStdGenAll (stdEnumGens :: StandardGens (Zipper Label))  genLowCharAll

genList_Int = substStdGenAll ((listStdGens A) :: StandardGens [Label])  genIntAll

-- genZip makes a pair generator from two generators
genValZip_Char r = 
  let gc = (genUpperCharRnd (mkStdGen 657985498)) -- random value generator
      gz = genAll genZip_Char -- exhaustive zipper generator
  in zip (gc 1) (gz r) -- (Char, Zipper Char) generator with random Char, exhaustive zippers

testFoldrz_1 r n = 
  let propFoldrz_str = propFoldrz (:) "" :: Property (Zipper Char)
      lbl = "Compare concatenating strings with right fold over char zipper and char list, up to rank " ++ (show r)
  in  stdTestArgs genZip_Char lbl r propFoldrz_str n

testFoldlz_1 r n = 
  let propFoldlz_sum = propFoldlz (+) 0 :: Property [Int]
      lbl = "Compare sum over integer list to left fold over integer zippers, up to rank " ++ (show r)
  in  stdTestArgs genList_Int lbl r propFoldlz_sum n

testInsDelRepl_1 r = 
  let lbl = "Compare insert/delete to replace, up to rank " ++ (show r)
      -- do up to maxr tests for zippers up to rank maxr
      instruct maxr = [(r, toInteger maxr) | r <- [1..maxr]] 
      suite = genSuite genValZip_Char (instruct r)
  in  simpleTest lbl propInsDelRepl suite
