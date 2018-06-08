{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
-- |
-- Module:      Data.HeytingAlgebra.Class
-- Description: HeytingAlgebra type class
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
-- case by case basis, however, they do not hold universally as they do with
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

    -- ** Other Operations
    , xor
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


-- | /Heyting algebra/ is defined as a distributive bounded lattice expanded
-- with an operation @∀ p q. p → q@ ('implication', '-->'). Importantly, every
-- /Boolean algebra/ is a /Heyting algebra/ when 'implication' ('-->') is
-- defined as:
--
-- @
-- ∀ a b. a '-->' b ≡ 'neg' a '\/' b
-- @
--
-- === Axioms and Laws
--
-- ==== Commutativity of \/\\ and \\\/
--
-- @
-- ∀ a b. a '\/' b = b '\/' a
-- ∀ a b. a '/\' b = b '/\' a
-- @
--
-- ==== Associativity of \/\\ and \\\/
--
-- @
-- ∀ a b. a '\/' (b '\/' c) = (a '\/' b) '\/' c
-- ∀ a b. a '/\' (b '/\' c) = (a '/\' b) '/\' c
-- @
--
-- ==== Absorbtion for \/\\ and \\\/
--
-- @
-- ∀ a b. a '\/' (a '/\' b) = a
-- ∀ a b. a '/\' (a '\/' b) = a
-- @
--
-- ==== Idempotent \/\\ and \\\/
--
-- @
-- ∀ a. a '\/' a  = a
-- ∀ a. a '/\' a  = a
-- @
--
-- ==== Identities for \/\\ and \\\/
--
-- @
-- ∀ a. a '\/' 'false' = 'false' '\/' a ≡ a
-- ∀ a. a '/\' 'true'  = 'false' '\/' a ≡ a
-- @
--
-- ==== Distributivity for \/\\ and \\\/
--
-- Heyting algebra is a distributive lattice:
--
-- @
-- ∀ a b c. a '\/' (b '/\' c) = (a '\/' b) '/\' (b '\/' c)
-- ∀ a b c. a '/\' (b '\/' c) = (a '/\' b) '\/' (b '/\' c)
-- @
--
-- ==== Implication
--
-- Distributive bounded lattice is a Heyting algebra iff:
--
-- @
-- ∀ a. a '-->' a = 'true'
-- ∀ a b. a '/\' (a '-->' b) = a '/\' b
-- ∀ a b. b '/\' (a '-->' b) = b
-- ∀ a b c. a '-->' (b '/\' c) = (a '-->' b) '/\' (a '-->' c)    (distributivity)
-- @
--
-- Ordering operation can be recovered from 'implication' ('-->') as:
--
-- @
-- ∀ a b. a ≤ b ⇔ a '-->' b = 1
-- @
--
-- ==== Negation
--
-- Negation is not a basic operation, it's defined as:
--
-- @
-- ∀ a. 'neg' a = a '-->' 'false'
-- @
class HeytingAlgebra a where
    {-# MINIMAL
            bottom
          , top
          , (negation | implication)
          , (conjunction | disjunction)
      #-}

    -- | Least element, behaves as neutral element for '\/' ('disjunction').
    --
    -- @
    -- ∀ a b. 'bottom' ≤ a '\/' b                (infimum)
    -- ∀ a. a '\/' 'bottom' = 'bottom' '\/' a ≡ a    (identity)
    -- 'bottom' :: 'Bool' = 'False'
    -- @
    --
    -- Aliases: 'ff' and 'false'.
    --
    -- If instance for 'Prelude.Bounded' is consistent with latice ordering,
    -- then:
    --
    -- @
    -- 'bottom' = 'Prelude.minBound'
    -- @
    bottom :: a

    -- | Greatest element, behaves as neutral element for '/\' ('conjunction').
    --
    -- @
    -- ∀ a b. a '/\' b ≤ 'top'             (supremum)
    -- ∀ a. a '/\' 'top' = 'top' '/\' a = a    (identity)
    -- 'top' :: 'Bool' = 'True'
    -- @
    --
    -- Aliases: 'tt' and 'true'.
    --
    -- If instance for 'Prelude.Bounded' is consistent with latice ordering,
    -- then:
    --
    -- @
    -- 'top' = 'Prelude.maxBound'
    -- @
    top :: a

    -- | Operation of negation, also known as complement, but in Heyting
    -- Algebra it's more known as pseudo-complement, because
    -- @∀ a. a '/\' 'neg' a ≡ 'top'@ (law of excluded middle) is not generally
    -- true. Bear in mind that there are Heyting algebras in which that is
    -- true. Every Boolean algebra is Heyting algebra and for those law of
    -- excluded middle holds.
    --
    -- Note that @∀ a. 'neg' ('neg' a) = a@ (double negation) doesn't hold
    -- either, since it's equivalent to the law of excluded middle.
    --
    -- Aliases: 'neg' and 'Data.HeytingAlgebra.not'.
    --
    -- Default implementation: @\\a -> a '-->' 'false'@
    negation :: a -> a
    negation = (`implication` bottom)

    -- | Operation of conjunction, or also meet or infimum when we think about
    -- lattice structure of Heyting algebra.
    --
    -- @
    -- ∀ a b. a '/\' b = a '/\' b                    (commutativity),
    -- ∀ a b c. a '/\' (b '/\' c) = (a '/\' b) '/\' c    (associativity)
    -- ∀ a. a '/\' a = a                           (idempotency).
    -- @
    --
    -- Aliases: '/\' and 'Data.HeytingAlgebra.&&'.
    --
    -- Default implication: @\\a b -> 'neg' ('neg' a '\/' 'neg' b)@
    conjunction :: a -> a -> a
    conjunction a b = negation (negation a `disjunction` negation b)

    -- | Operation of conjunction, or also join or supremum when we think about
    -- lattice structure of Heyting algebra.
    --
    -- @
    -- ∀ a b. a '\/' b = a '\/' b                    (commutativity),
    -- ∀ a b c. a '\/' (b '\/' c) = (a '\/' b) '\/' c    (associativity)
    -- ∀ a. a '\/' a = a                           (idempotency).
    -- @
    --
    -- Aliases: '\/' and 'Data.HeytingAlgebra.||'.
    --
    -- Default implication: @\\a b -> 'neg' ('neg' a '/\' 'neg' b)@
    disjunction :: a -> a -> a
    disjunction a b = negation (negation a `conjunction` negation b)

    -- | Operation of implication.
    --
    -- Aliases: '-->'
    --
    -- Default implementation: @\\a b -> 'neg' (a '\/' b)@
    implication :: a -> a -> a
    implication a b = negation (a `disjunction` b)

    -- | Operation of equivalence, i.e. biconditional. See also 'xor'
    -- (exclusive or) operation.
    --
    -- @
    -- a '<-->' b = (a '-->' b) '/\' (b '-->' a)
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

-- | Exclusive OR operation.
--
-- @
-- ∀ a b. a ``xor`` b = 'neg' (a '<-->' b) = (a '\/' b) '/\' 'neg' (a '/\' b)
-- @
--
-- Defined as: @\\a b -> (a '\/' b) '/\' 'neg' (a '/\' b)@
xor :: HeytingAlgebra a => a -> a -> a
xor a b = (a `disjunction` b) `conjunction` negation (a `conjunction` b)
