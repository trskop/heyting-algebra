{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
-- |
-- Module:      Data.HeytingAlgebra.Any
-- Description: TODO: Module synopsis
-- Copyright:   (c) 2018 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions.
--
-- TODO
module Data.HeytingAlgebra.Any
    (
      Any(..)

    -- * Folds
    , or
    , any
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


newtype Any a = Any {getAny :: a}
  deriving (Eq, Generic, Ord, Read, Show)

instance Eq1 Any where
    liftEq = coerce

instance Ord1 Any where
    liftCompare = coerce

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
