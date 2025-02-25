{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Main where

import qualified Data.List as L
import Data.Hashable
import Control.Monad.Identity
import qualified Arith as A

import qualified Data.HashMap.Strict as M
import OpOrdering
import DSL
import WQO as WQO
import MultisetOrder as MultisetOrder
import Nat
import RPO as RPO
import KBO as KBO
import StrictOC as StrictOC
import LazyOC as LazyOC
import SMT as SMT
import qualified QuickCheckTests as QuickCheckTests
import System.IO

import Language.REST.ExploredTerms
import Language.REST.OCAlgebra
import Language.REST.OCToAbstract
import Language.REST.Core
import Language.REST.LPO (lpo)
import Language.REST.KBO (kbo)
import Language.REST.WQOConstraints as OC
import Language.REST.Op
import Language.REST.RPO
import Language.REST.Internal.OpOrdering
import Language.REST.RuntimeTerm
import Language.REST.MetaTerm as MT
import Language.REST.Internal.Rewrite
import Language.REST.Rest
import Language.REST.SMT
import qualified Language.REST.WQOConstraints.ADT as AC
import Language.REST.Internal.WorkStrategy
import qualified Data.Maybe as Mb
import qualified Data.HashSet as S

diverges :: (Show oc) => OCAlgebra oc RuntimeTerm IO -> [RuntimeTerm] -> IO Bool
diverges impl ts = not <$> (isSat impl $ orient impl ts)

rewrites :: (Show oc, Hashable oc, Eq oc)
  => OCAlgebra oc RuntimeTerm IO
  -> S.HashSet Rewrite -> S.HashSet Rewrite -> RuntimeTerm -> IO (S.HashSet RuntimeTerm)
rewrites impl evalRWs userRWs t0 =
  terms <$> fst <$> rest
    RESTParams
      { re           = evalRWs
      , ru           = userRWs
      , toET         = id
      , target       = Nothing
      , workStrategy = notVisitedFirst
      , ocImpl       = ?impl
      , initRes      = termsResult
      , etStrategy   = ExploreWhenNeeded
      } t0
  where
    ?impl = impl

runTest :: (String, IO Bool) -> IO ()
runTest (name, test) = do
  putStr $ "Running " ++ name ++ "... "
  hFlush stdout
  ok <- test
  if ok
    then putStrLn "Passed"
    else error $ "Test " ++ name ++ " FAILED"

class Testable a where
  toTest :: a -> IO Bool

instance Testable Bool where
  toTest = return

instance Testable (IO Bool) where
  toTest = id

runTestSuite :: Testable a => String -> [(String, a)] -> IO ()
runTestSuite name tests1 = do
  putStrLn $ "Running test suite: " ++ name
  mapM_ (runTest . go) tests1
  where
    go (name1, test) = (name1, toTest test)


orderingTests :: (Hashable (oc Op), Show (oc Op), Ord (oc Op)) => (?impl :: WQOConstraints oc IO) => [(String, IO Bool)]
orderingTests =
  [
    ("simple1", return $ not $ (rpoGTE "f(t1)" "g(t2)") `permits'` (t1Op =. t2Op))
  , ("simple2", return $ (rpoGTE "f(t1)" "g(t2)") `permits'` (Mb.fromJust $ merge (f >. g) (t1Op =. t2Op)))
  , ("simple3", return $ (rpoGTE "f(t1)" "g(t2)") `permits'` (Mb.fromJust $ merge (f >. g) (t1Op >. t2Op)))
  , ("subterm", return $ rpoGTE "f(g)" "f" == noConstraints ?impl)
  , ("intersect", OC.isUnsatisfiable ?impl $ OC.intersect ?impl (OC.singleton ?impl (f  >. g)) (OC.singleton ?impl (g >. f)))
  ]
  where
    permits' = permits ?impl

proveEQ :: (Show oc, Hashable oc, Eq oc) =>
     OCAlgebra oc RuntimeTerm IO
  -> S.HashSet Rewrite -> S.HashSet Rewrite
  -> RuntimeTerm -> RuntimeTerm -> IO Bool
proveEQ impl evalRWs userRWs have want =
  do
    rw1 <- (rewrites impl evalRWs userRWs have)
    rw2 <- (rewrites impl evalRWs userRWs want)
    return $ not $ disjoint rw1 rw2
  where
    disjoint s1 s2 = S.null $ s1 `S.intersection` s2

arithTests :: (Show oc, Hashable oc, Eq oc) => OCAlgebra oc RuntimeTerm IO -> [(String, IO Bool)]
arithTests impl =
  [
    ("Contains", return $ contains (intToTerm 2) (intToTerm 1))
  , ("Diverge", not <$> (diverges impl [ (intToTerm 2) .+ t1
                               , (intToTerm 1) .+ t1
                               ]
                    ))
  , ("Diverge3", not <$> (diverges impl [ (t1 .+ t2) .+ t3
                               , t1 .+ (t2 .+ t3)
                               , (t2 .+ t3) .+ t1
                               ]
                    ))
  , ("Eval1", arithEQ (intToTerm 2 .+ intToTerm 3) 5)
  , ("Eval2", arithEQ (ack (intToTerm 3) (intToTerm 2)) 29)
  , ("Subst1", return $ subst (M.fromList [("x", intToTerm 1), ("y", intToTerm 2)]) (x #+ y) == (intToTerm 1 .+ intToTerm 2))
  , ("ArithTerm", termTest)
  , ("ArithTerm2", termTest2)
  , ("Arith0", eq (t1 .+ t2 .+ intToTerm 1) (t1 .+ (intToTerm 1 .+ t2)))
  , ("Arith1", eq (intToTerm 2 .+ intToTerm 3) (intToTerm 3 .+ intToTerm 2))
  , ("Arith2", eq (t1 .+ t2) (t2 .+ t1))
  , ("Arith3", not <$> eq (t2 .+ t1) (t2 .+ t2))
  , ("Arith4", eq ((t1 .+ t2) .+ t3) (t1 .+ (t2.+ t3)))
  , ("Arith4.1", eq (t1 .+ t2 .+ t3) (t3 .+ t2 .+ t1))
  , ("Arith5", eq (zero .+ t1) t1)
  , ("Arith5.1", eq (zero .+ zero .+ t1) t1)
  , ("Arith5.2", eq (zero .+ zero .+ zero .+ t1) t1)
  , ("Arith6", eq (((t1 .+ t1) .+ t3) .+ t4) (t1 .+ (t1 .+ (t3 .+ t4))))
  , ("Arith7", eq ((intToTerm 2 .+ intToTerm 1) .+ t1) (intToTerm 2 .+ (intToTerm 1 .+ t1)))
  ]
  where

    arithEQ t n = do
      t' <- eval A.evalRWs t
      return $ termToInt t' == Just n


    termTest = proveEQ impl evalRWs userRWs (App f1 [t1]) zero
      where
        evalRWs = S.union termEvalRWs  A.evalRWs
        userRWs = S.insert (MT.RWApp g1 [x] ~> MT.RWApp f1 [x]) A.userRWs
        termEvalRWs = S.fromList
          [  MT.RWApp f1 [x] ~> MT.RWApp g1 [suc' x]
          ,  MT.RWApp g1 [x] ~> zero'
          ]
        f1 = Op "f"
        g1 = Op "g"

    termTest2 = proveEQ impl evalRWs userRWs (App f1 [zero]) (App g1 [zero])
      where
        evalRWs = S.union termEvalRWs A.evalRWs
        userRWs = S.insert (MT.RWApp f1 [x] ~> MT.RWApp g1 [(suc' (suc' x))]) A.userRWs
        termEvalRWs = S.fromList
          [  MT.RWApp f1 [suc' x] ~> MT.RWApp g1 [suc' x]
          ,  MT.RWApp f1 [zero']  ~> zero'
          ,  MT.RWApp g1 [suc' x] ~> MT.RWApp f1 [x]
          ,  MT.RWApp g1 [zero']  ~> zero'
          ]
        f1 = Op "f"
        g1 = Op "g"


    eq = proveEQ impl A.evalRWs A.userRWs

completeTests :: (Show oc, Hashable oc, Eq oc) => OCAlgebra oc RuntimeTerm IO -> [(String, IO Bool)]
completeTests impl =
  [ ("CompleteDiverges", not <$> diverges impl [App start [], App mid [], App finish []])
  , ("Complete1"     , eq (App start []) (App finish []))
  , ("EvalComplete2" , (== (App finish [])) <$> eval completeUserRWs (App start' [App s1 []]) )
  , ("Complete2"     , eq (App start' [App s1 []]) (App finish []))
  ]
  where

    completeUserRWs = S.fromList
      [ MT.RWApp start [] ~> MT.RWApp mid []
      , MT.RWApp mid []   ~> MT.RWApp finish []

      , MT.RWApp start' [MT.RWApp s2 []] ~> MT.RWApp mid' [MT.RWApp s1 []]
      , MT.RWApp s1 []                 ~> MT.RWApp s2 []
      , MT.RWApp mid' [MT.RWApp s2 []]   ~> MT.RWApp finish []
      ]

    eq :: RuntimeTerm -> RuntimeTerm -> IO Bool
    eq = proveEQ impl S.empty completeUserRWs

    start  = Op "start"
    mid    = Op "mid"
    finish = Op "finish"

    start'  = Op "start'"
    mid'    = Op "mid'"

    s1      = Op "s1"
    s2      = Op "s2"

ocTests :: (Handle, Handle) -> IO ()
ocTests z3 = do
  runTestSuite "LazyOC" LazyOC.tests
  runTestSuite "StrictOC" StrictOC.tests
  runTestSuite "RPO" RPO.tests
  runTestSuite "Ordering" orderingTests
  where
    ?impl = AC.adtOC z3

main :: IO ()
main = spawnZ3 >>= go where

  implTests implName impl toSkip = do
    runTestSuite ("Arith" ++ implName) (withSkips $ arithTests impl)
    runTestSuite ("Complete" ++ implName) (withSkips $ completeTests impl)
    where
      withSkips tests1 = do
        (name, test) <- tests1
        guard $ L.notElem name toSkip
        return (name, test)


  go :: SolverHandle -> IO ()
  go z3 =
    do
      putStrLn "Running REST Test Suite"
      runTestSuite "SMT" SMT.tests
      runTestSuite "KBO" (KBO.tests z3)
      _ <- QuickCheckTests.tests
      runTestSuite "OpOrdering" OpOrdering.tests
      ocTests z3
      runTestSuite "MultisetOrder" MultisetOrder.tests
      runTestSuite "WQO" WQO.tests
      implTests "KBO" (kbo z3) []
      implTests "RPO" (lift (AC.adtOC z3) rpo) []
      implTests "LPO" (lift (AC.adtOC z3) lpo) ["Diverge3", "Arith4", "Arith4.1", "Arith6"]
      killZ3 z3
