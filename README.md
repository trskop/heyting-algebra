# Heyting Algebra

[![Haskell Programming Language](https://img.shields.io/badge/language-Haskell-blue.svg)](http://www.haskell.org)
[![BSD3 License](http://img.shields.io/badge/license-BSD3-brightgreen.svg)](https://tldrlegal.com/license/bsd-3-clause-license-%28revised%29)


This package was inspired by `Data.HeytingAlgebra` module in
[`purescript-prelude`](https://pursuit.purescript.org/packages/purescript-prelude).

We can think of *Heyting algebra* as a generalisation of `Bool` and its boolean
operations. The advantage is that we get other instances for `HeytingAlgebra`
that are useful in practical programming.

*Heyting algebra* is a formalisation of *Intuitionistic logic* which can be
seen as a restriction of classical logic in which the law of excluded middle
and double negation elimination have been removed. Excluded middle and double
negation elimination can still be proved for some propositions on a case by
case basis, however, they do not hold universally as they do with classical
logic.
