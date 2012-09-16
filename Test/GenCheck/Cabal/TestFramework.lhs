\begin{code}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeFamilies #-}

module Test.GenCheck.Cabal.TestFramework where

import Control.Monad (liftM)
import Data.Maybe (catMaybes, fromJust, maybe)
import Data.Typeable (Typeable(..))
import qualified Distribution.TestSuite as Cabal
import System.Random (newStdGen, next, StdGen)

import           Test.GenCheck as GC
import           Test.GenCheck.System.Result as GCR
import           Test.GenCheck.Base.Datum as GCD
import           Test.GenCheck.Base.Verdict as GCV

\end{code}

\begin{code}
data GCTest = forall a. GC.Testable a => GCTest String (Property a)

test :: GC.Testable a => String -> Property a -> Cabal.Test
test n p = Cabal.pure $ GCTest n p

\end{code}

\begin{code}

data SuiteType = Base | Std | Deep
    deriving (Eq, Read, Show, Typeable)

instance Cabal.TestOptions GCTest where
     name (GCTest n _) = n

     options _ =
         [ ("suite-type",  typeOf (undefined :: SuiteType))
         , ("label",       typeOf (undefined :: String))
         , ("std-gen",     typeOf (undefined :: String))
         , ("max-rank",    typeOf (undefined :: Int))
         , ("num-tests",   typeOf (undefined :: Int))
         ]

     defaultOptions _ = do
         rng <- newStdGen
         return $ Cabal.Options $
             [ ("suite-type",  show Std)            
             , ("label",       dlbl)
             , ("std-gen",     show rng)
             , ("max-rank",    show 30)
             , ("num-tests",   show 100)
             ]
         where dlbl = "GenCheck Default Check: Standard suite, rank 30, 100 tests"

     check t (Cabal.Options opts) = catMaybes
         [ maybeNothing "suite-type"  ([] :: [(Int, String)])
         , maybeNothing "max-rank"    ([] :: [(Int, String)])
         , maybeNothing "num-tests"   ([] :: [(Int, String)])
         ]
         -- There is no need to check the parsability of "label" or "std-gen"
         -- because the Read instance for String and StdGen always succeeds.
         where
             maybeNothing n x =
                 maybe Nothing (\str ->
                     if reads str == x then Just n else Nothing)
                     $ lookup n opts

\end{code}

\begin{code}
instance Cabal.PureTestable GCTest where
    run (GCTest n prop) opts = 
      let res = cabalTestRun prop suite
      in  if (GCV.summaryverdict res) then Cabal.Pass
          else Cabal.Fail "blew it"
      where 
          suite     = GC.stdSuite stdgs seed maxRank count
          stdgs     = GC.stdTestGens
--          lbl       = Cabal.lookupOption "label" opts
          seed      = Cabal.lookupOption "std-gen" opts
          maxRank   = Cabal.lookupOption "max-rank" opts
          count     = Cabal.lookupOption "num-tests" opts
{-
instance Cabal.ImpureTestable GCTest where
     runM (GCTest _ prop) o =
         catch go (return . Cabal.Error . show)
         where
             go = return Cabal.Pass
             go = do
                 result <- cabalTestImpure lbl prop gc_suite
                 return $ if (GCR.result result) 
                         then Cabal.Pass
                         else Cabal.Fail $ "failed"

             gc_suite = case (Cabal.lookupOption "suite-type" o) of
                          Std  -> GC.stdSuite  GC.stdTestGens seed maxRank count
                          Base -> GC.baseSuite GC.stdTestGens              count
                          Deep -> GC.deepSuite GC.stdTestGens seed maxRank count

             lbl       = Cabal.lookupOption "label" o
             seed      = Cabal.lookupOption "std-gen" o
             maxRank   = Cabal.lookupOption "max-rank" o
             count     = Cabal.lookupOption "num-tests" o
-}
\end{code}

Simple test results are stored in a |SimpleTestPt| structure,
and include only the test case and boolean property value.
|SimpleTestPt| is an instance of both Verdict and Datum,
which allow access to the test result and input value respectively.

\begin{code}

--cabalTestImpure :: Show a => String -> Property a -> MapRankSuite a -> IO (Bool)
--cabalTestImpure lbl p ts = dspVerdict lbl $ cabalTestRun p ts

cabalTestRun :: Property a -> MapRankSuite a -> CabalResults a
cabalTestRun p ts = fmap (Prelude.map (cabalTestCase p)) ts

type CabalResults a = MapRankSuite (CabalTestPt a)
 
cabalTestCase :: Property a -> a -> CabalTestPt a
cabalTestCase p x = Pt (p x) x

data CabalTestPt a = Pt Bool a
instance Show a => Show (CabalTestPt a) where
  show (Pt True x) = show x
  show (Pt False x) = "FAILED: " ++ (show x)

instance Datum (CabalTestPt a) where
  type DataType (CabalTestPt a) = a
  datum (Pt _ x) = x  
instance Verdict (CabalTestPt a) where
  verdict (Pt b _) = b

\end{code}



