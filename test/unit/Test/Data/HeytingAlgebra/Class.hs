{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module:      Test.Data.HeytingAlgebra.Class
-- Description: TODO: Module synopsis
-- Copyright:   (c) 2018 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions.
--
-- TODO: Module description.
module Test.Data.HeytingAlgebra.Class (tests)
  where

import Data.Bool (Bool)
import qualified Data.Bool as Bool
import Data.Coerce (coerce)
import Data.Function (($), const)

import Data.Functor.Contravariant (Equivalence(..), Predicate(..))

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperties)
import Test.QuickCheck ((===), property)

import Data.HeytingAlgebra.Class (HeytingAlgebra(..))


tests :: TestTree
tests = testGroup "Data.HeytingAlgebra.Class"
    [ testInstanceBool
    , testInstanceArrow
    , testInstancePredicate
    , testInstanceEquivalence
    ]

-- | Check that we are consistent with 'Bool' operations.
testInstanceBool :: TestTree
testInstanceBool = testProperties "Bool"
    [ ( "negation"
      , property $ \a ->
          negation a === Bool.not a
      )
    , ( "conjunction"
      , property $ \a b ->
          conjunction a b === (a Bool.&& b)
      )
    , ( "disjunction"
      , property $ \a b ->
          disjunction a b === (a Bool.|| b)
      )
    , ( "implication"
      , property $ \a b ->
          implication a b === (Bool.not a Bool.|| b)
      )
    , ( "disjunction"
      , property $ \a b ->
          biconditional a b === (implication a b Bool.&& implication b a)
      )
    ]

-- | We know that 'Bool' works, now we have to check that functions map to
-- correct operations.
testInstanceArrow :: TestTree
testInstanceArrow = testProperties "() -> Bool"
    [ ( "negation"
      , property $ \a ->
          pred1 negation a === Bool.not a
      )
    , ( "conjunction"
      , property $ \a b ->
          pred2 conjunction a b === (a Bool.&& b)
      )
    , ( "disjunction"
      , property $ \a b ->
          pred2 disjunction a b === (a Bool.|| b)
      )
    , ( "implication"
      , property $ \a b ->
          pred2 implication a b === (Bool.not a Bool.|| b)
      )
    , ( "disjunction"
      , property $ \a b ->
          pred2 biconditional a b
            === (implication a b Bool.&& implication b a)
      )
    ]
  where
    pred1 :: ((() -> Bool) -> () -> Bool) -> Bool -> Bool
    pred1 f a = f (const a) ()

    pred2 :: ((() -> Bool) -> (() -> Bool) -> () -> Bool) -> Bool -> Bool -> Bool
    pred2 f a b = f (const a) (const b) ()

testInstancePredicate :: TestTree
testInstancePredicate = testProperties "Predicate ()"
    [ ( "negation"
      , property $ \a ->
          pred1 negation a === Bool.not a
      )
    , ( "conjunction"
      , property $ \a b ->
          pred2 conjunction a b === (a Bool.&& b)
      )
    , ( "disjunction"
      , property $ \a b ->
          pred2 disjunction a b === (a Bool.|| b)
      )
    , ( "implication"
      , property $ \a b ->
          pred2 implication a b === (Bool.not a Bool.|| b)
      )
    , ( "disjunction"
      , property $ \a b ->
          pred2 biconditional a b
            === (implication a b Bool.&& implication b a)
      )
    ]
  where
    pred1
        :: (Predicate () -> Predicate ())
        -> Bool -> Bool
    pred1 f a = (coerce f (\() -> a) :: () -> Bool) ()

    pred2
        :: (Predicate () -> Predicate () -> Predicate ())
        -> Bool -> Bool -> Bool
    pred2 f a b = (coerce f (\() -> a) (\() -> b) :: () -> Bool) ()

testInstanceEquivalence :: TestTree
testInstanceEquivalence = testProperties "Equivalence ()"
    [ ( "negation"
      , property $ \a ->
          equiv1 negation a === Bool.not a
      )
    , ( "conjunction"
      , property $ \a b ->
          equiv2 conjunction a b === (a Bool.&& b)
      )
    , ( "disjunction"
      , property $ \a b ->
          equiv2 disjunction a b === (a Bool.|| b)
      )
    , ( "implication"
      , property $ \a b ->
          equiv2 implication a b === (Bool.not a Bool.|| b)
      )
    , ( "disjunction"
      , property $ \a b ->
          equiv2 biconditional a b
            === (implication a b Bool.&& implication b a)
      )
    ]
  where
    equiv1
        :: (Equivalence () -> Equivalence ())
        -> Bool -> Bool
    equiv1 f a = (coerce f (\() () -> a) :: () -> () -> Bool) () ()

    equiv2
        :: (Equivalence () -> Equivalence () -> Equivalence ())
        -> Bool -> Bool -> Bool
    equiv2 f a b = (coerce f (\() () -> a) (\() () -> b) :: () -> () -> Bool) () ()
