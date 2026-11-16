{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Main where

import Data.Hashable
import qualified Data.Maybe as Mb
import qualified ExploredTerms
import OpOrdering
import WQO
import MultisetOrder
import Nat
import RPO
import KBO
import SMT
import qualified QuickCheckTests
import System.IO

import Language.REST.WQOConstraints as OC
import qualified Language.REST.WQOConstraints.Strict as SC
import Language.REST.Op
import Language.REST.RPO
import Language.REST.Internal.OpOrdering
import Language.REST.Internal.WQO
import Language.REST.SMT

-- Helpers formerly provided by the DSL module.
f, g, t1Op, t2Op :: Op
f    = Op "f"
g    = Op "g"
t1Op = Op "t1"
t2Op = Op "t2"

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
    ("simple1", return $ not $ rpoGTE "f(t1)" "g(t2)" `permits'` (t1Op =. t2Op))
  , ("simple2", return $ rpoGTE "f(t1)" "g(t2)" `permits'` Mb.fromJust (merge (f >. g) (t1Op =. t2Op)))
  , ("simple3", return $ rpoGTE "f(t1)" "g(t2)" `permits'` Mb.fromJust (merge (f >. g) (t1Op >. t2Op)))
  , ("subterm", return $ rpoGTE "f(g)" "f" == noConstraints ?impl)
  , ("intersect", OC.isUnsatisfiable ?impl $ OC.intersect ?impl (OC.singleton ?impl (f >. g)) (OC.singleton ?impl (g >. f)))
  ]
  where
    permits' = permits ?impl

main :: IO ()
main = spawnZ3 >>= \z3 -> do
  putStrLn "Running REST Test Suite"
  runTestSuite "ExploredTerms" ExploredTerms.tests
  runTestSuite "SMT" SMT.tests
  runTestSuite "KBO" (KBO.tests z3)
  _ <- QuickCheckTests.tests
  runTestSuite "OpOrdering" OpOrdering.tests
  runTestSuite "RPO" RPO.tests
  runTestSuite "Ordering" orderingTests
  runTestSuite "MultisetOrder" MultisetOrder.tests
  runTestSuite "WQO" WQO.tests
  killZ3 z3
  where
    ?impl = SC.strictOC
