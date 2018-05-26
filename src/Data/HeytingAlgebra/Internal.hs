{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
-- |
-- Module:      Data.HeytingAlgebra.Internal
-- Description: TODO: Module synopsis
-- Copyright:   (c) 2018 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions.
--
-- TODO
module Data.HeytingAlgebra.Internal
    ( (#.)
    , coerce2
    )
  where

import Data.Coerce (Coercible, coerce)


(#.) :: Coercible b c => (b -> c) -> (a -> b) -> (a -> c)
(#.) _ = coerce
{-# INLINE (#.) #-}

coerce2
  :: forall f t a b
  .  Coercible (f a -> f b) (t a -> t b)
  => (f a -> f b)
  -> (t a -> t b)
coerce2 = coerce
{-# INLINE coerce2 #-}
