{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
-- |
-- Module:      Data.HeytingAlgebra
-- Description: Heyting Algebra
-- Copyright:   (c) 2018 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions.
--
-- We can think of /Heyting algebra/ as a generalisation of 'Bool' and its
-- boolean operations. The advantage is that we get other instances for
-- 'HeytingAlgebra' that are useful in practical programming. See
-- [HeytingAlgebra Instances](#g:2) section for some examples.
module Data.HeytingAlgebra
    (
    -- * Heyting Algebra
    --
    -- $heytingAlgebra
      module Data.HeytingAlgebra.Class

    -- * HeytingAlgebra Instances

    -- ** HeytingAlgebra Bool
    --
    -- $boolInstance

    -- ** Other Instances
    --
    -- $otherInstances
    , module Data.HeytingAlgebra.All
    , module Data.HeytingAlgebra.Any

    -- * Other Aliases
    , not
    , (&&)
    , (||)

    -- * Utilities
    , fromBool
    , toBool
    )
  where

import Data.Bool (Bool(False, True), otherwise)
import Data.Eq (Eq, (==))
import Data.Maybe (Maybe(Just, Nothing))

import Data.HeytingAlgebra.All
import Data.HeytingAlgebra.Any
import Data.HeytingAlgebra.Class


-- | Alias for 'negation' operation of 'HeytingAlgebra'. Clashes with standard
-- definition of 'Data.Bool.not' in @base@ package.
not :: HeytingAlgebra a => a -> a
not = negation
{-# INLINE not #-}

-- | Alias for 'conjunction' operation of 'HeytingAlgebra'. Clashes with
-- standard definition of 'Data.Bool.&&' in @base@ package.
(&&) :: HeytingAlgebra a => a -> a -> a
(&&) = conjunction
infixr 3 &&
{-# INLINE (&&) #-}

-- | Alias for 'disjunction' operation of 'HeytingAlgebra'. Clashes with
-- standard definition of 'Data.Bool.&&' in @base@ package.
(||) :: HeytingAlgebra a => a -> a -> a
(||) = disjunction
infixr 2 ||
{-# INLINE (||) #-}

-- | Monotonic injection from 'Bool' into any 'HeytingAlgebra', i.e. preserves
-- boolean operations.
fromBool :: HeytingAlgebra a => Bool -> a
fromBool = \case
    False -> bottom
    True -> top
{-# INLINE fromBool #-}

-- | Projection from 'HaytingAlgebra' into three-value set.
--
-- @
-- ∀ a. 'HeytingAlgebra' a => 'toBool' @a . 'fromBool' @a = 'Just' @Bool
--
-- ∀ (a :: Type) (b :: Type). ('HeytingAlgebra' a, 'HeytingAlgebra' b) =>
--   'fromBool' \@b ('toBool' \@a 'false') = 'Just' \@b 'false'
--   'fromBool' \@b ('toBool' \@a 'true')  = 'Just' \@b 'true'
--   'fromBool' \@b ('toBool' \@a other) = 'Nothing' \@b
-- @
toBool :: (Eq a, HeytingAlgebra a) => a -> Maybe Bool
toBool a
  | a == bottom = Just False
  | a == top    = Just True
  | otherwise   = Nothing
{-# INLINEABLE toBool #-}

-- $heytingAlgebra
--
-- 'HeytingAlgebra' defines special elements:
--
-- @
-- 'bottom' ≡ 'false' ≡ 'ff' :: 'HaytingAlgebra' => a
-- 'top' ≡ 'true' ≡ 'tt' :: 'HaytingAlgebra' => a
-- @
--
-- And following operations:
--
-- @
-- 'negation' ≡ 'neg' ≡ 'not' :: 'HaytingAlgebra' => a -> a
-- 'conjunction' ≡ ('/\') ≡ ('&&') :: 'HaytingAlgebra' => a -> a -> a
-- 'disjunction' ≡ ('\/') ≡ ('||') :: 'HaytingAlgebra' => a -> a -> a
-- 'implication' ≡ ('-->') :: 'HaytingAlgebra' => a -> a -> a
-- 'biconditional' ≡ ('<-->') :: 'HaytingAlgebra' => a -> a -> a
-- @

-- $boolInstance
--
-- 'Bool' type with 'Data.Bool.not', 'Data.Bool.&&', and 'Data.Bool.||' is
-- the most basic example of 'HeytingAlgebra':
--
-- @
-- 'bottom' ≡ 'false' ≡ 'ff' ≡ 'False' :: 'Bool'
--
-- 'top' ≡ 'true' ≡ 'tt' ≡ 'True' :: 'Bool'
--
-- 'negation' ≡ 'neg' ≡ 'Data.Bool.not' :: 'Bool' -> 'Bool'
--   where
--     'not' 'False' = 'True'
--     'not' 'True'  = 'False'
--
-- 'conjunction' ≡ ('/\') ≡ ('Data.Bool.&&') :: 'Bool' -> 'Bool' -> 'Bool'
--   where
--     'False' '&&' 'False' = 'False'
--     'False' '&&' 'True'  = 'False'
--     'True'  '&&' 'False' = 'False'
--     'True'  '&&' 'True'  = 'True'
--
-- 'disjunction' ≡ ('\/') ≡ ('Data.Bool.||') :: 'Bool' -> 'Bool' -> 'Bool'
--   where
--     'False' '||' 'False' = 'False'
--     'False' '||' 'True'  = 'True'
--     'True'  '||' 'False' = 'True'
--     'True'  '||' 'True'  = 'True'
--
-- 'implication' ≡ ('-->') :: 'Bool' -> 'Bool' -> 'Bool'
--   where
--     ∀ (a :: Bool) (b :: Bool).
--       a '-->' b = 'not' a '||' b
--
--     i.e.:
--       'False' '-->' 'False' = 'True'
--       'False' '-->' 'True'  = 'True'
--       'True'  '-->' 'False' = 'False'
--       'True'  '-->' 'True'  = 'True'
--
-- 'biconditional' ≡ ('<-->') :: 'Bool' -> 'Bool' -> 'Bool'
--   where
--     ∀ (a :: Bool) (b :: Bool).
--       a '<-->' b = (a '-->' b) '&&' (b '-->' a)
--
--     i.e.:
--       'False' '-->' 'False' = 'True'
--       'False' '-->' 'True'  = 'False'
--       'True'  '-->' 'False' = 'False'
--       'True'  '-->' 'True'  = 'True'
-- @

-- $otherInstances
--
-- Important instances:
--
-- [@instance 'HeytingAlgebra' b => 'HeytingAlgebra' (a -> b)@]:
--   Simple predicates, e.g.
--   @
--   isCrOrLf = ('==' \'\\r\') '\/' ('==' \'\\n\') :: 'Data.Char.Char' -> 'Bool'
--   @
--
-- [@instance 'HeytingAlgebra' ('Data.Functor.Contravariant.Predicate' a)@]:
--   Same as @'HeytingAlgebra' b => 'HeytingAlgebra' (a -> b)@, but wrapped in
--   a newtype called 'Data.Functor.Contravariant.Predicate'.
--
-- [@instance 'HeytingAlgebra' ('Data.Functor.Contravariant.Equivalence' a)@]:
--   Same as
--   @'HeytingAlgebra' b => 'HeytingAlgebra' (a -> 'Data.Functor.Contravariant.Predicate' a)@,
--   but wrapped in a newtype called 'Data.Functor.Contravariant.Equivalence'.
--
-- [@instance 'HeytingAlgebra' a => 'HeytingAlgebra' ('All' a)@]:
--   Monoid under `conjunction` (`/\`, `Data.HeytingAlgebra.&&`) with `top`
--   (`true`) as neutral element.
--
-- [@instance 'HeytingAlgebra' a => 'HeytingAlgebra' ('Any' a)@]:
--   Monoid under `disjunction` (`\/`, `Data.HeytingAlgebra.||`) with `bottom`
--   (`false`) as neutral element.
