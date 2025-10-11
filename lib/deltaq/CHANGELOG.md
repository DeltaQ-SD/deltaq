# Revision history for `deltaq`

## 1.1.0.0 — to be released

### Added

* Add `DeltaQ.Diagram` for rendering outcome expressions as diagrams.
* Add `DeltaQ.Expr` with a data type `O` for outcome expressions.

### Changed

* Add the new class `ProbabilisticOutcome` and remove its methods
  `Probability`, `choice` and `choices` from the `DeltaQ` class.

## 1.0.1.0 — 2025-09-26

### Added

* Add combinator `timeout` that returns probabilities conditioned
  on completing within a given duration.
* Add functions `retryOverlap` and `retryOverlapN` for modelin retry policies
  that race time-delayed, independent copies of an outcome against each other.
* Clarify semantics of `deadline`.
* Bump dependencies, allow `containers-0.8`, `QuickCheck-2.16`, `optparse-applicative-0.19`.

### Fixed

* Fix `quantile` to return the correct result when the result is precisely at the boundary between two pieces of the piecewise probability distribution.

## 1.0.0.0 — 2024-12-23

* Initial release of the domain-specific language for ∆Q System Development.
* Implementation based on `probability-polynomial`.
