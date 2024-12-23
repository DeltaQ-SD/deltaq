# Complexity Management Strategy

# Problem Statement

The section [Performance measurements](https://github.com/DeltaQ-SD/deltaq/blob/41e75562843dcad0eae2d10fc678f7efb7c11a17/doc/reports/artefact-A2.md#performance-measurements)
of the [Report on test results, coverage analysis and performance](https://github.com/DeltaQ-SD/deltaq/blob/41e75562843dcad0eae2d10fc678f7efb7c11a17/doc/reports/artefact-A2.md)
highlights that repeated application of sequential composition, `.>>.`, creates
probability distributions with large internal representation size. (The internal
representation size is measured by the function `complexity`.)

Specifically, sequential composition of `N` outcomes, `o1 .>>. o2 .>>. … .>>.on`,
will give a probability distribution whose internal representation size is
**exponential** in `N`. This means that computing the result will become
exponentially slow as the number of outcomes increases. As such compositions
represent typical use cases, this slowdown is a **problem** for **usability**.

For instance, given the following definitions:

```hs
mix :: Int -> DQ
mix m = choice 0.5 (wait 0) (wait (2^(m :: Int)))


convolved :: Int -> DQ
convolved n = foldr1 (.>>.) $ map mix [1..n]
```

We get the following results for `complexity . convolved`:

| n  | time | complexity | complexity / sec | 
|:---:|:---:|:---:|:----:|
| 19 | 0.908s                    | 524288  | 577193.3  |
| 20 | 1.79s                     | 1048576 | 585958.8  |
| 21 | 3.56s                     | 2097152 | 588980.8  |
| 22 | 7.17s                     | 4194304 | 585338.8  |
| 23 | 14.3s                     | 8388608 | 586128.25 |
| 24 | 28.2s                     | 16777216 | 594332.6 |
| 25 | 56.79s                    | 33554432 | 590929   |


Unfortunately, the problem is inherent in the **semantics** of **sequential composition**,
which requires combining every possible delay of the first
outcome with every possible delay of the second. For instance, consider
statistical mixtures of discrete Dirac measures at the powers of two,

```
import Numeric.Measure.Discrete
import qualified Data.Map as Map

mix :: Int -> Discrete Rational
mix m = scale 0.5 $ add (dirac 0) (dirac (2^(m :: Int)))
```

Then, the n-fold convolution

```
convolved :: Int -> Discrete Rational
convolved n = foldr1 convolve $ map mix [1..n]
```

will have internal representation size `2^n`. For example

```
>>> Map.size $ toMap $ convolved 10
1024
```

# Solution Proposals

The problem statement indicates that an exact representation of probability
distributions will invariably lead to exponentially growing internal
representation size. There are several approaches to addressing the resulting
usability problem:

1. Maintaining the representation of a distribution function mapping time to
   probability, but using some form of approximation to reduce the complexity;
2. Exploit non-strict evaluation: this may be effective where only certain
   functions of the distribution are of interest, such as the certain quantiles
   or success probabilities in relation to a QTA;
3. Use Laplace transforms in relation to derived quantities that are linear in
   the transformed space, such as min/max values and moments;
4. Exploit the algebraic properties of outcome expressions to simplify the
   evaluation.

## Common requirements for approximation

It is essential that any approximation comes with an **estimate** of the
introduced **error**, so that the veracity of the eventual result can be
assessed. In particular, the propagation of this error under the operations
needs to be described. Moreover, the **direction** in which **error**
accumulates should be controllable, so that whether the results are ‘optimistic’
or ‘pessimistic’ can be decided on the basis of the needs of the modelling. This
will require some sophisticated exploration, tailored to the important use-cases
of the tool. There is unlikely to be a one-size-fits-all approach that is
appropriate for all cases.

For example, the library can provide a function

```
approximate :: DeltaQ o => Precision -> o -> o
```

such that `approximate eps o` returns a probability distribution with reduced
internal size that is within precision `eps` of the original distribution `o` in
some suitably defined metric. The direction of the error can be imposed by a
condition such as

```
∀ t eps o. successWithin t (approximate eps o) <= successWithin t o
```

which corresponds to a ‘pessimistic’ approximation which always decreases
timeliness compared to the original.

The ‘closeness’ of the approximation can be formulated in terms of the
complexity of the ‘experiments’ required to distinguish them, and — if two
distributions can only be distinguished using a very computationally intensive
procedure, they can be considered essentially equivalent. This can be considered
in the context of the infidelity/junk already present in the modelling.

## Potential approximation approaches

### Removing Tails

The pathological example above has a long tail of decreasing probability steps
at increasing time intervals. As we are typically interested in defined limits
on timeliness, it makes sense to truncate such a tail after a given maximum time
*Tmax*. Moreover, we may also have a predefined limit for the probability of
failure, so we can truncate the tail after a given level of probability has been
reached, *Pmax \= 1 \- ε*.

### Approximating Polynomials

In the current representation, convolution of two distributions involving
polynomial functions of degree bound *n* results in one with polynomials of
degree bound *2n*. Multiple convolutions thus produce polynomials of very high
degree. We could switch the representation of polynomials to a different basis
such as Chebychev polynomials, which offer favorable conditions for
approximation. More radically, we could replace a polynomial on an interval with
a linear interpolation between the ends of the interval. If the interval is
short, the error introduced will be small.

### Merging Intervals

Some intervals may contribute little to the overall probability and could be
merged until the increment in probability reaches a specified threshold. Whether
the overall probability increment should be placed at the beginning or end of
the merged interval (as a step) or spread uniformly (as a linear interpolation)
should depend on the desired direction of error accumulation.

### Discretising probability

A more radical approach is to approximate in probability, i.e. consider the
cumulative probabilities of 44.999% and 50.000% approximately equal. In other
words, the idea is to approximate the cumulative distribution function by a step
function with few steps. This has advantages over the typical approach of
quantizing the time dimension, in that the quantisation range is finite.

Polynomials would have to be discretised similarly, e.g. explode a polynomial
into steps. This does result in a massive increase of representation size for
any given polynomial, but for sufficiently fine-grained discretisation, a
saturation effect sets in, where the results of operations do not increase the
internal representation size anymore.

### t-Digests

Another radical approach is to approximate distributions with quantiles or
t-digests ([https://arxiv.org/abs/1902.04023](https://arxiv.org/abs/1902.04023)
). How to perform the various operations using this representation would require
further study.

## Non-strict evaluation

In the context of evaluating some fitness function, it may be possible to keep
the full representation but refrain from evaluating the long tails by exploiting
non-strict evaluation. This may require analysis of the propagation of demand
through the various operators.

## Laplace transforms

Transforming the representation can make certain properties monoidal, such as
`earliest`, `deadline` and `moments`. This is true for moments of convolved
distributions, since means and variances simply add. Further investigation is
required to ensure this also holds for the remaining operations. This approach
can provide rapid evaluation during an initial assessment, where the
distribution tail is of less immediate interest, for instance determining
whether an outcome can be delivered even on average.

## Algebraic transformations

Published papers provide a set of algebraic transformations of outcome
expressions, which could be exploited to minimise the complexity of evaluating
the final expression. These can also be used to extract certain values such as
the overall failure probability.

# Conclusion and next steps

The tool in its current form is suitable for investigating systems up to a
certain complexity scale. The performance is perfectly adequate for tutorial
examples and straightforward models, for which it gives exact distributions.
However, it may be problematic in the context of higher-order operations such as
fixed-point extraction or exploration of multi-dimensional parameter spaces.

Given an abstract syntax tree representation of an outcome expression, a
low-complexity evaluator of monoidal functions such as moments can be written
that avoids evaluation of the full expression. This also facilitates the
application of algebraic transformations, removal of common sub-expressions,
elimination of redundant terms and so on, and so constitutes an important next
step.
