{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
-- |
-- Module:      Data.HeytingAlgebra.All
-- Description: All data type which is a monoid under conjunction
-- Copyright:   (c) 2018 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions.
--
-- `All` data type which is a monoid under `conjunction` ('/\',
-- 'Data.HeytingAlgebra.&&') with `top` (`true`) as neutral element.
module Data.HeytingAlgebra.All
    (
      All(..)

    -- * Folds
    , and
    , all
    )
  where

import Data.Coerce (coerce)
import Data.Eq (Eq)
import Data.Foldable (Foldable, foldMap)
import Data.Functor (Functor(..))
import Data.Functor.Classes (Eq1(..), Ord1(..))
import Data.Functor.Identity (Identity(Identity))
import Data.Monoid (Monoid(..))
import Data.Ord (Ord)
import Data.Semigroup (Semigroup(..))
import GHC.Generics (Generic)
import Text.Read (Read)
import Text.Show (Show)

import Data.HeytingAlgebra.Class
import Data.HeytingAlgebra.Internal


-- | /Heyting Algebra/ monoid under `conjunction` (`/\`,
-- `Data.HeytingAlgebra.&&`) with `top` (`true`) as neutral element.
newtype All a = All {getAll :: a}
  deriving (Eq, Generic, Ord, Read, Show)

instance Eq1 All where
    liftEq = coerce

instance Ord1 All where
    liftCompare = coerce

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

-- | Conjunction of all emenents of a container.
--
-- >>> and ([] :: [Bool])
-- True
-- >>> and [True, False]
-- False
-- >>> and [odd, even] 1
-- False
and :: (Foldable t, HeytingAlgebra a) => t a -> a
and = getAll #. foldMap All

-- | Determines whether all elements of the structure satisfy the predicate.
all :: (Foldable t, HeytingAlgebra b) => (a -> b) -> t a -> b
all f = getAll #. foldMap (All #. f)
