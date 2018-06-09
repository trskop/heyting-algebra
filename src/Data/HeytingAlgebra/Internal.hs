{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
-- |
-- Module:      Data.HeytingAlgebra.Internal
-- Description: Internal helper functions
-- Copyright:   (c) 2018 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions.
--
-- Internal helper functions used to simplify and optimise other parts of the
-- code base.
module Data.HeytingAlgebra.Internal
    ( (#.)
    , coerce2
    )
  where

import Data.Coerce (Coercible, coerce)


-- | **Unsafe function, use with caution!**
(#.) :: Coercible b c => (b -> c) -> (a -> b) -> (a -> c)
(#.) _ = coerce
{-# INLINE (#.) #-}

-- | **Unsafe function, use with caution!**
coerce2
  :: forall f t a b
  .  Coercible (f a -> f b) (t a -> t b)
  => (f a -> f b)
  -> (t a -> t b)
coerce2 = coerce
{-# INLINE coerce2 #-}
