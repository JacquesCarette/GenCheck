This is an installation test for the GenCheck programs after a Cabal install.

\begin{code}
module Main where

import System.Random(newStdGen)
import System.Exit(exitSuccess)

import Test.HUnit

import Test.GenCheck.System.Result as GCR

import Test.GenCheck.Tests.TestReverseList
import Test.GenCheck.Tests.Test_BaseInt
import Test.GenCheck.Tests.Test_SortBinT

\end{code}

Reverse list tests.

\begin{code}
testRL1 = TestCase $ assertBool "reverse involution test" (GCR.result testRevRev1)
testRU1 = TestCase $ assertBool "reverse unit equality test" (GCR.result testRevUnit1)
testRF1 = TestCase $ assertBool "must fail, rev xs == xs" 
                                (not (GCR.result testRevFail1))
testsRev = TestList 
  [ TestLabel "reverse involution" testRL1
  , TestLabel "Testing singleton reverse equality" testRU1
  , TestLabel "Test should fail, rev xs == xs" testRF1
  ]

\end{code}

Integer (base type) test.

\begin{code}
testBase1 n = TestCase $ assertBool "fail - under 100" 
                           (not (GCR.result (testIntLT100_fail n)))

testsBase n = TestList 
  [ TestLabel "Catch failure for all Ints < 100" (testBase1 n)
  ]

\end{code}

Sorting a binary tree.

\begin{code}
testSort1 s r n = TestCase $ assertBool "random node binary tree sort" 
                             (GCR.result (testBTSort_RandNode s r n))

testSort2 s r n = TestCase $ assertBool "extreme node binary tree sort" 
                             (GCR.result (testBTSort_XtrmNode s r n))
testSortFail s r n = TestCase $ assertBool "random node unsorted should fail"
                           (not (GCR.result (failBTSort_RandNode s r n)))

testsSort (s1,s2,s3) r n = TestList
 [ TestLabel "Sort random node binary tree" $ testSort1 s1 r n
 , TestLabel "Sort extreme valued node binary tree" $ testSort2 s2 r n
 , TestLabel "Catch unsorted random node binary tree" $ testSortFail s3 r n
 ]

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
          exitSuccess

\end{code}
