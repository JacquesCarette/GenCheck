This is an installation test for the GenCheck programs after a Cabal install.

\begin{code}
module Main where

import System.Random(mkStdGen)
import System.Exit(exitSuccess, exitFailure)

import Test.HUnit

import Test.GenCheck.System.Result as GCR

import Test.GenCheck.Tests.TestReverseList
import Test.GenCheck.Tests.Test_BaseInt
import Test.GenCheck.Tests.SortBinTree.Test_SortBinT
import Test.GenCheck.Tests.ListZipper.TestListZipper

boolTestCase lbl test = 
  do putStrLn lbl
     r <- test
     putStrLn "-- end of test result --"
     return $ GCR.result r

boolFalseTestCase lbl test = 
  do putStrLn lbl
     r <- test
     putStrLn "-- end of test result --"
     return $ not (GCR.result r)

\end{code}

Reverse list tests.

\begin{code}
testRL1 = boolTestCase "reverse involution test"      $ testRevRev1  100
testRU1 = boolTestCase "reverse unit equality test"   $ testRevUnit1 100
testRF1 = boolFalseTestCase "must fail, rev xs == xs" $ testRevFail1 100
testsRev = do {r1<- testRL1; r2 <- testRU1; r3 <- testRF1; return (r1 && r2 && r3) }

\end{code}

Integer (base type) test.

\begin{code}
testBase1 = boolFalseTestCase "fail - under 100" (testIntLT100_fail 100)

testsBase =  testBase1

\end{code}

Testing a zippered list structure (from Tutorial 2)

\begin{code}
testListZip1 r n = boolTestCase "Compare concatenating strings with right fold"
                        (testFoldrz r n)
testListZip2 r n = boolTestCase "Compare sum over integer list to left fold"
                        (testFoldlz r n)
testListZip3 r =     boolTestCase "compare insert / delete with replace"
                        (testInsDelRepl r)

testsListZip r n = 
  do r1 <- testListZip1 10 100  -- 100 tests up to size 10
     r2 <- testListZip2 10 100
     r3 <- testListZip3 10      -- exhaustively test all cursor positions up to size 10
     return (r1 && r2 && r3)

\end{code}

Sorting a binary tree.

\begin{code}
testSort1 s r n = boolTestCase "random node binary tree sort" 
                         (testBTSort_RandNode s r n)

testSort2 s r n = boolTestCase "extreme node binary tree sort" 
                         (testBTSort_XtrmNode s r n)
testSortFail s r n = boolFalseTestCase "random node unsorted should fail"
                         (failBTSort_RandNode s r n)

testsSort = 
  do r1 <- testSort1    (mkStdGen 65498435) 10 300  -- 300 tests up to rank 10
     r2 <- testSort2    (mkStdGen  8798328) 10 300   -- with seeds for random node generation
     r3 <- testSortFail (mkStdGen   126879) 10 300 
     return (r1 && r2 && r3)

\end{code}

Main test program.

\begin{code}
main :: IO Counts
main = do r1 <- testsRev
          r2 <- testsBase
          r3 <- testsListZip 10 100
          r4 <- testsSort
          if (r1 && r2 && r3 && r4) then exitSuccess else exitFailure

\end{code}
