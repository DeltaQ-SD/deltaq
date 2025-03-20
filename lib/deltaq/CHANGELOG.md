# Revision history for `deltaq`

## 1.0.1.0 — to be released

### Added

* Add combinator `timeout` that returns probabilities conditioned
  on completing within a given duration.
* Add functions `retryOverlap` and `retryOverlapN` for modelin retry policies
  that race time-delayed, independent copies of an outcome against each other.

## 1.0.0.0 — 2024-12-23

* Initial release of the domain-specific language for ∆Q System Development.
* Implementation based on `probability-polynomial`.
