# Overview

∆Q System Development is a paradigm for developing distributed systems that meet performance requirements. ∆Q System Development uses cheap statistical models as part of the design process in order to rule out infeasible designs early on. In this way, we avoid the high cost of implementing a system design first and finding out that the system is unuseable in reality later.

∆Q System Development works by defining desired system outcomes, exploring different designs that decompose these outcomes into lower-level outcomes, and estimating their performance characteristics.

∆Q System Development represents performance as a probability distribution of the expected time to complete an outcome. This repository includes a Haskell package `probability-polynomial` to concretely represent and manipulate such distributions, and a package `deltaq` (pronounced "Delta Q") built on top of this for writing outcome expressions and evaluating their performance characteristics.

For more information on the ∆Q System Development paradigm, also consult

* https://www.pnsol.com/publications.html
* https://www.youtube.com/@pnsol

# Contents

This repository is structured as follows:

* `lib/` — Haskell packages
    * `deltaq` — Library for writing outcome expressions and evaluating their performance characteristics.
    * `probability-polynomial` — Library for representing probability distributions on the half-line $[0,+∞)$. Used for the implementation of `deltaq`.
* `docs/`
    — Documentation with worked examples.

# QuickStart

Prerequisites:

* [Haskell toolchain](https://www.haskell.org/downloads/) with `cabal` and `ghc`.

We recommend to start with the examples in the `docs` directory and consulting the API documentation of the module `DeltaQ`. 

To generate the API documentation, use

```hs
cabal haddock all
```

To build the packages, use

```hs
cabal build all
```

To run the tests for the `deltaq` package, use

```hs
cabal test deltaq
```

# Thanks

Development of this package has been generously supported by [IOG](https://iog.io)
and PLWORKZ R&D.

# Roadmap

* Approximation for probability distributions of large size
* Create more examples
