This is an installation test for the GenCheck programs after a Cabal install.

\begin{code}
module Main where

import System.Random(newStdGen)
import System.Exit(exitSuccess)

import Test.HUnit

import Test.GenCheck.System.Result as GCR

import Test.GenCheck.Tests.TestReverseList
import Test.GenCheck.Tests.Test_BaseInt
import Test.GenCheck.Tests.SortBinTree.Test_SortBinT
import Test.GenCheck.Tests.ListZipper.TestListZipper

boolTestCase lbl test = 
  TestLabel lbl $ TestCase $ assertBool lbl (GCR.result test)
boolFalseTestCase lbl test = 
  TestLabel lbl $ TestCase $ assertBool lbl (not (GCR.result test))

\end{code}

Reverse list tests.

\begin{code}
testRL1 = boolTestCase "reverse involution test"      testRevRev1
testRU1 = boolTestCase "reverse unit equality test"   testRevUnit1
testRF1 = boolFalseTestCase "must fail, rev xs == xs" testRevFail1
testsRev = TestList [ testRL1, testRU1, testRF1 ]

\end{code}

Integer (base type) test.

\begin{code}
testBase1 n = boolFalseTestCase "fail - under 100" (testIntLT100_fail n)

testsBase n = TestList [ (testBase1 n) ]

\end{code}

Sorting a binary tree.

\begin{code}
testSort1 s r n = boolTestCase "random node binary tree sort" 
                         (testBTSort_RandNode s r n)

testSort2 s r n = boolTestCase "extreme node binary tree sort" 
                         (testBTSort_XtrmNode s r n)
testSortFail s r n = boolFalseTestCase "random node unsorted should fail"
                         (failBTSort_RandNode s r n)

testsSort (s1,s2,s3) r n = TestList
 [ testSort1 s1 r n, testSort2 s2 r n, testSortFail s3 r n ]

\end{code}

Testing a zippered list structure (from Tutorial 2)

\begin{code}
testListZip1 s r n = boolTestCase "Compare concatenating strings with right fold"
                        (testFoldrz s r n)
testListZip2 s r n = boolTestCase "Compare sum over integer list to left fold"
                        (testFoldlz s r n)
testListZip3 r =     boolTestCase "compare insert / delete with replace"
                        (testInsDelRepl r)

testsListZip (s1,s2) r n = TestList 
  [ testListZip1 s1 r n, testListZip2 s2 r n, testListZip3 r]
\end{code}

Main test program.

\begin{code}
main :: IO Counts
main = do runTestTT testsRev
          runTestTT (testsBase 100)
          s <- newStdGen
          s2 <- newStdGen
          s3 <- newStdGen
          runTestTT $ testsSort (s,s2,s3) 10 300
          s4 <-newStdGen
          s5 <- newStdGen
          runTestTT $ testsListZip (s4,s5) 10 100
          exitSuccess

\end{code}
