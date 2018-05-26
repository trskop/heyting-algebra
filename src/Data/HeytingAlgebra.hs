{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
-- |
-- Module:      Data.HeytingAlgebra
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
module Data.HeytingAlgebra
    ( HeytingAlgebra(..)

    , ff
    , false

    , tt
    , true

    , not
    , neg

    , (&&)
    , (/\)

    , (||)
    , (\/)

    , (-->)
    , (<-->)

    , All(..)
    , and
    , all

    , Any(..)
    , or
    , any
    )
  where

import Data.Bool (Bool(False, True))
import qualified Data.Bool as Bool ((&&), (||), not)
import Data.Coerce (Coercible, coerce)
import Data.Foldable (Foldable, foldMap)
import Data.Function ((.), const)
import Data.Functor (Functor(..))
import Data.Functor.Identity (Identity(Identity))
import Data.Functor.Const (Const(Const))
import Data.Monoid (Monoid(..))
import Data.Semigroup (Semigroup(..))

import Data.Functor.Contravariant
  ( Equivalence(Equivalence)
  , Predicate(Predicate)
  )


-- | /Heyting algebra/ is defined as a bounted lattice with 'implication'
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
    -- ∀ a. a '\/' 'bottom' ≡ 'bottom' '\/' a ≡ a
    -- 'bottom' :: 'Bool' ≡ 'False'
    -- @
    --
    -- Aliases: 'ff' and 'false'.
    bottom :: a

    -- | Greatest element, behaves as neutral element for '/\' ('conjunction').
    --
    -- @
    -- ∀ a. a '/\' 'top' ≡ 'top' '/\' a ≡ a
    -- 'top' :: 'Bool' ≡ 'True'
    -- @
    --
    -- Aliases: 'tt' and 'true'.
    top :: a

    -- |
    -- Aliases: 'neg' and 'not'.
    negation :: a -> a

    -- |
    --
    -- Aliases: '/\' and '&&'.
    conjunction :: a -> a -> a

    -- |
    -- Aliases: '\/' and '||'.
    disjunction :: a -> a -> a

    -- |
    -- Aliases: '-->'
    implication :: a -> a -> a
    implication a b = negation (a `disjunction` b)

    -- |
    -- Aliases: '<-->'
    biconditional :: a -> a -> a
    biconditional a b =
        (a `conjunction` b) `disjunction` (negation a `conjunction` negation b)
        -- = (a /\ b) \/ (-.a /\ -.b) = (a --> b) /\ (b --> a)

instance HeytingAlgebra Bool where
    bottom = False
    top = True
    negation = Bool.not
    conjunction = (Bool.&&)
    disjunction = (Bool.||)
    implication a b = Bool.not a Bool.|| b

instance HeytingAlgebra () where
    bottom = ()
    top = ()
    negation = const ()
    conjunction _ _ = ()
    disjunction _ _ = ()
    implication _ _ = ()

instance HeytingAlgebra b => HeytingAlgebra (Const b a) where
    bottom = coerce (bottom @b)
    top = coerce (top @b)
    negation = coerce (negation @b)
    conjunction = coerce (conjunction @b)
    disjunction = coerce (disjunction @b)
    implication = coerce (implication @b)

instance HeytingAlgebra b => HeytingAlgebra (a -> b) where
    bottom = const bottom
    top = const bottom
    negation = (negation .)
    conjunction f g a = f a `conjunction` g a
    disjunction f g a = f a `disjunction` g a
    implication f g a = f a `implication` g a

instance HeytingAlgebra (Predicate a) where
    bottom = coerce (bottom @(a -> Bool))
    top = coerce (top @(a -> Bool))
    negation = coerce (negation @(a -> Bool))
    conjunction = coerce (conjunction @(a -> Bool))
    disjunction = coerce (disjunction @(a -> Bool))
    implication = coerce (implication @(a -> Bool))

instance HeytingAlgebra (Equivalence a) where
    bottom = coerce (bottom @(a -> Predicate a))
    top = coerce (top @(a -> Predicate a))
    negation = coerce (negation @(a -> Predicate a))
    conjunction = coerce (conjunction @(a -> Predicate a))
    disjunction = coerce (disjunction @(a -> Predicate a))
    implication = coerce (implication @(a -> Predicate a))

ff :: HeytingAlgebra a => a
ff = bottom

false :: HeytingAlgebra a => a
false = bottom

tt :: HeytingAlgebra a => a
tt = top

true :: HeytingAlgebra a => a
true = top

not :: HeytingAlgebra a => a -> a
not = negation

neg :: HeytingAlgebra a => a -> a
neg = negation

(&&) :: HeytingAlgebra a => a -> a -> a
(&&) = conjunction
infixr 3 &&

(||) :: HeytingAlgebra a => a -> a -> a
(||) = disjunction
infixr 2 ||

(/\) :: HeytingAlgebra a => a -> a -> a
(/\) = conjunction
infixr 3 /\

(\/) :: HeytingAlgebra a => a -> a -> a
(\/) = disjunction
infixr 2 \/

(-->) :: HeytingAlgebra a => a -> a -> a
(-->) = implication
infixr 1 -->

(<-->) :: HeytingAlgebra a => a -> a -> a
(<-->) = biconditional
infixr 0 <-->

-- {{{ All --------------------------------------------------------------------

newtype All a = All {getAll :: a}

instance Functor All where
    fmap = coerce2 @Identity #. fmap
        -- coerce2 is actually discarded and the coerce in (#.) is used.

instance HeytingAlgebra a => Semigroup (All a) where
    (<>) = coerce (conjunction @a)

instance HeytingAlgebra a => Monoid (All a) where
    mempty = coerce (top @a)
    mappend = (<>)

instance HeytingAlgebra a => HeytingAlgebra (All a) where
    bottom = coerce (bottom @a)
    top = coerce (top @a)
    negation = coerce (negation @a)
    conjunction = coerce (conjunction @a)
    disjunction = coerce (disjunction @a)
    implication = coerce (implication @a)

and :: (Foldable t, HeytingAlgebra a) => t a -> a
and = getAll #. foldMap All

all :: (Foldable t, HeytingAlgebra b) => (a -> b) -> t a -> b
all f = getAll #. foldMap (All #. f)

-- }}} All --------------------------------------------------------------------

-- {{{ Any --------------------------------------------------------------------

newtype Any a = Any {getAny :: a}

instance Functor Any where
    fmap = coerce2 @Identity #. fmap
        -- coerce2 is actually discarded and the coerce in (#.) is used.

instance HeytingAlgebra a => Semigroup (Any a) where
    (<>) = coerce (disjunction @a)

instance HeytingAlgebra a => Monoid (Any a) where
    mempty = coerce (bottom @a)
    mappend = (<>)

instance HeytingAlgebra a => HeytingAlgebra (Any a) where
    bottom = coerce (bottom @a)
    top = coerce (top @a)
    negation = coerce (negation @a)
    conjunction = coerce (conjunction @a)
    disjunction = coerce (disjunction @a)
    implication = coerce (implication @a)

or :: (Foldable t, HeytingAlgebra a) => t a -> a
or = getAny #. foldMap Any

any :: (Foldable t, HeytingAlgebra b) => (a -> b) -> t a -> b
any f = getAny #. foldMap (Any #. f)

-- }}} Any --------------------------------------------------------------------

(#.) :: Coercible b c => (b -> c) -> (a -> b) -> (a -> c)
(#.) _ = coerce

coerce2
  :: forall f t a b
  .  Coercible (f a -> f b) (t a -> t b)
  => (f a -> f b)
  -> (t a -> t b)
coerce2 = coerce
