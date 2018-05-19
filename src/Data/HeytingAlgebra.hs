{-# LANGUAGE NoImplicitPrelude #-}
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
-- TODO: Module description.
module Data.HeytingAlgebra
--  (
--  )
  where

import Data.Bool (Bool(False, True))
import qualified Data.Bool as Bool ((&&), (||), not)
import Data.Function ((.), const)


class HeytingAlgebra a where
    bottom :: a
    top :: a
    negation :: a -> a
    conjunction :: a -> a -> a
    disjunction :: a -> a -> a

    implication :: a -> a -> a
    implication a b = negation (a `disjunction` b)

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

instance HeytingAlgebra b => HeytingAlgebra (a -> b) where
    bottom = const bottom
    top = const bottom
    negation = (negation .)
    conjunction f g a = f a `conjunction` g a
    disjunction f g a = f a `disjunction` g a
    implication f g a = f a `implication` g a

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
