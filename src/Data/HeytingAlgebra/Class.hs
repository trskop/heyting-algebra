{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
-- |
-- Module:      Data.HeytingAlgebra.Class
-- Description: TODO: Module synopsis
-- Copyright:   (c) 2018 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions.
--
-- /Heyting algebra/ is a formalisation of /Intuitionistic logic/ which can be
-- seen as a restriction of classical logic in which the law of excluded middle
-- and double negation elimination have been removed. Excluded middle and
-- double negation elimination can still be proved for some propositions on a
-- case by case basis, however, but do not hold universally as they do with
-- classical logic.
--
-- == Related information
--
-- * [Wikipedia: Heyting algebra](https://en.wikipedia.org/wiki/Heyting_algebra)
-- * [Wikipedia: Intuitionistic logic](https://en.wikipedia.org/wiki/Intuitionistic_logic)
module Data.HeytingAlgebra.Class
    (
    -- * Heyting Algebra
      HeytingAlgebra(..)

    -- ** Aliases
    , ff
    , false

    , tt
    , true

    , neg
    , (/\)
    , (\/)
    , (-->)
    , (<-->)
    )
  where

import Data.Bool (Bool(False, True))
import qualified Data.Bool as Bool ((&&), (||), not)
import Data.Coerce (coerce)
import Data.Function ((.), const)
import Data.Functor.Identity (Identity(Identity))
import Data.Functor.Const (Const(Const))

import Data.Functor.Contravariant
  ( Equivalence(Equivalence)
  , Predicate(Predicate)
  )


-- | /Heyting algebra/ is defined as a bounded lattice with 'implication'
-- ('-->') operator. Importantly, every /Boolean algebra/ is a
-- /Heyting algebra/ when 'implication' ('-->') is defined as:
--
-- @
-- ∀ a b. a '-->' b ≡ 'neg' a '\/' b
-- @
--
-- == Axioms
--
-- === Commutativity of \/\\ and \\\/
--
-- @
-- ∀ a b. a '\/' b ≡ b '\/' a
-- ∀ a b. a '/\' b ≡ b '/\' a
-- @
--
-- === Associativity of \/\\ and \\\/
--
-- @
-- ∀ a b. a '\/' (b '\/' c) ≡ (a '\/' b) '\/' c
-- ∀ a b. a '/\' (b '/\' c) ≡ (a '/\' b) '/\' c
-- @
--
-- === Absorbtion for \/\\ and \\\/
--
-- @
-- ∀ a b. a '\/' (a '/\' b) ≡ a
-- ∀ a b. a '/\' (a '\/' b) ≡ a
-- @
--
-- === Idempotent \/\\ and \\\/
--
-- @
-- ∀ a. a '\/' a  ≡ a
-- ∀ a. a '/\' a  ≡ a
-- @
--
-- === Identities for \/\\ and \\\/
--
-- @
-- ∀ a. a '\/' 'false' ≡ 'false' '\/' a ≡ a
-- ∀ a. a '/\' 'true'  ≡ 'false' '\/' a ≡ a
-- @
--
-- === Implication
--
-- @
-- ∀ a. a '-->' a ≡ 'true'
-- ∀ a b. a '/\' (a '-->' b) ≡ a '/\' b
-- ∀ a b. b '/\' (a '-->' b) ≡ b
-- ∀ a b c. a '-->' (b '/\' c) ≡ (a '-->' b) '/\' (a '-->' c)
-- @
--
-- @
-- ∀ a. 'neg' a ≡ a '-->' 'false'
-- @
class HeytingAlgebra a where
    {-# MINIMAL bottom, top, negation, conjunction, disjunction #-}

    -- | Least element, behaves as neutral element for '\/' ('disjunction').
    --
    -- @
    -- ∀ a b. 'bottom' ≤ a '\/' b
    -- ∀ a. a '\/' 'bottom' ≡ 'bottom' '\/' a ≡ a
    -- 'bottom' :: 'Bool' ≡ 'False'
    -- @
    --
    -- Aliases: 'ff' and 'false'.
    bottom :: a

    -- | Greatest element, behaves as neutral element for '/\' ('conjunction').
    --
    -- @
    -- ∀ a b. a '/\' b ≤ 'top'
    -- ∀ a. a '/\' 'top' ≡ 'top' '/\' a ≡ a
    -- 'top' :: 'Bool' ≡ 'True'
    -- @
    --
    -- Aliases: 'tt' and 'true'.
    top :: a

    -- |
    -- Aliases: 'neg' and 'Data.HeytingAlgebra.not'.
    negation :: a -> a

    -- |
    --
    -- Aliases: '/\' and 'Data.HeytingAlgebra.&&'.
    conjunction :: a -> a -> a

    -- |
    -- Aliases: '\/' and 'Data.HeytingAlgebra.||'.
    disjunction :: a -> a -> a

    -- |
    -- Aliases: '-->'
    --
    -- Default implementation: @\\a b -> 'neg' (a '\/' b)@
    implication :: a -> a -> a
    implication a b = negation (a `disjunction` b)

    -- |
    --
    -- @
    -- a '<-->' b ≡ (a '-->' b) '/\' (b '-->' a)
    -- @
    --
    -- Aliases: '<-->'
    --
    -- Default implementation: @\\a b -> (a '-->' b) '/\' (b '-->' a)@
    biconditional :: a -> a -> a
    biconditional a b = (a `implication` b) `conjunction` (b `implication` a)

instance HeytingAlgebra Bool where
    bottom = False
    top = True
    negation = Bool.not
    conjunction = (Bool.&&)
    disjunction = (Bool.||)
    implication a b = Bool.not a Bool.|| b
    biconditional a b = (a Bool.&& b) Bool.|| (Bool.not a Bool.&& Bool.not b)
      -- = (a /\ b) \/ (-.a /\ -.b) = (a --> b) /\ (b --> a) = a <--> b

instance HeytingAlgebra () where
    bottom = ()
    top = ()
    negation = const ()
    conjunction _ _ = ()
    disjunction _ _ = ()
    implication _ _ = ()
    biconditional _ _ = ()

instance HeytingAlgebra b => HeytingAlgebra (a -> b) where
    bottom = const bottom
    top = const bottom
    negation = (negation .)
    conjunction f g a = f a `conjunction` g a
    disjunction f g a = f a `disjunction` g a
    implication f g a = f a `implication` g a
    biconditional f g a = f a `biconditional` g a

instance HeytingAlgebra a => HeytingAlgebra (Identity a) where
    bottom = coerce (bottom @a)
    top = coerce (top @a)
    negation = coerce (negation @a)
    conjunction = coerce (conjunction @a)
    disjunction = coerce (disjunction @a)
    implication = coerce (implication @a)
    biconditional = coerce (biconditional @a)

instance HeytingAlgebra b => HeytingAlgebra (Const b a) where
    bottom = coerce (bottom @b)
    top = coerce (top @b)
    negation = coerce (negation @b)
    conjunction = coerce (conjunction @b)
    disjunction = coerce (disjunction @b)
    implication = coerce (implication @b)
    biconditional = coerce (biconditional @b)

instance HeytingAlgebra (Predicate a) where
    bottom = coerce (bottom @(a -> Bool))
    top = coerce (top @(a -> Bool))
    negation = coerce (negation @(a -> Bool))
    conjunction = coerce (conjunction @(a -> Bool))
    disjunction = coerce (disjunction @(a -> Bool))
    implication = coerce (implication @(a -> Bool))
    biconditional = coerce (biconditional @(a -> Bool))

instance HeytingAlgebra (Equivalence a) where
    bottom = coerce (bottom @(a -> Predicate a))
    top = coerce (top @(a -> Predicate a))
    negation = coerce (negation @(a -> Predicate a))
    conjunction = coerce (conjunction @(a -> Predicate a))
    disjunction = coerce (disjunction @(a -> Predicate a))
    implication = coerce (implication @(a -> Predicate a))
    biconditional = coerce (biconditional @(a -> Predicate a))

-- | Alias for 'bottom', a least element of 'HeytingAlgebra'.
ff :: HeytingAlgebra a => a
ff = bottom

-- | Alias for 'bottom', a least element of 'HeytingAlgebra'.
false :: HeytingAlgebra a => a
false = bottom

-- | Alias for 'top', a greatest element of 'HeytingAlgebra'.
tt :: HeytingAlgebra a => a
tt = top

-- | Alias for 'top', a greatest element of 'HeytingAlgebra'.
true :: HeytingAlgebra a => a
true = top

-- | Alias for 'negation' operation of 'HeytingAlgebra'.
neg :: HeytingAlgebra a => a -> a
neg = negation

-- | Alias for 'conjunction' operation of 'HeytingAlgebra'.
(/\) :: HeytingAlgebra a => a -> a -> a
(/\) = conjunction
infixr 3 /\

-- | Alias for 'disjunction' operation of 'HeytingAlgebra'.
(\/) :: HeytingAlgebra a => a -> a -> a
(\/) = disjunction
infixr 2 \/

-- | Alias for 'implication' operation of 'HeytingAlgebra'.
(-->) :: HeytingAlgebra a => a -> a -> a
(-->) = implication
infixr 1 -->

-- | Alias for 'biconditional' operation of 'HeytingAlgebra'.
(<-->) :: HeytingAlgebra a => a -> a -> a
(<-->) = biconditional
infixr 0 <-->
