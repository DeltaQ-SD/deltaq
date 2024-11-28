{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-|
Module      : Convolutionclasses
Description : Class definition for operators with testable properties
Copyright   : (c) Peter Thompson, 2024
License     : BSD-2-Clause
Maintainer  : peter.thompson@pnsol.com
Stability   : experimental

The standard algebraic classes such as semiring do not have all the operators we need.
In particular we want integration and differentiation operators that satisfy the
Fundamental Theorem of Calculus and combine appropriately with addition and multiplication.
We also want a convolution operator that behaves correctly.
-}
module PWPs.PiecewiseClasses
    ( Integrable (..)
    , Differentiable (..)
    , StepIntegrable (..)
    , StepDifferentiable (..)
    , Evaluable (..)
    , CompactConvolvable (..)
    , Comparable (..)
    , Mergeable (..)
    , Displayable (..)
    , ComplexityMeasureable (..)
    )
where

import Numeric.Polynomial.Simple
import qualified Numeric.Polynomial.Simple as Poly

class Integrable a b where
    integrate :: a -> b
class StepIntegrable a b c where
    integrateStep :: b -> Either a c

class StepDifferentiable a b c where
    differentiateStep :: (a, b) -> c

class Differentiable a b where
    differentiate :: a -> b

class Evaluable a b where
    evaluate :: a -> b -> a -- evaluate b at point a
    boost :: a -> b -> b -- increment b by a
    scale :: a -> b -> b -- scale b by a

{-|
    Convolution in our library is over finite intervals - this is what piecewiseness needs
-}
class CompactConvolvable a b where
    convolveIntervals :: (a, a, b) -> (a, a, b) -> [(a, b)] -- convolution

{-|
    We express a partial order by comparing a pair of objects on an interval, delivering a Maybe Ordering
-}
class Comparable a b where
    compareObjects :: (a, a, (b, b)) -> Maybe Ordering

{-|
    We want to know when two objects can be merged - this is not the same as saying they are equal
    We also define a zeroObject to be used when there aren't two objects to be merged
-}
class Mergeable a where
    mergeObject :: a -> a -> Maybe a
    zero :: a

class Displayable a b where
    displayObject :: a -> (a, a, b) -> Either (a, a) [(a, a)]

class ComplexityMeasureable a where
    measureComplexity :: a -> Int

{-|
    Laws:
    Usual stuff with +, *, -
    differentiate . integrate = id              } Fundamental theorem
    integrate . differentiate = add constant    } of calculus
    times distributes over integration and differentiation
    addition distributes over everything
    convolution is commutative and associative
    differentiate (f <+> g) == (differentiate f) <+> g == f <+> (differentiate g)
    integrate (f <+> g) == (integrate f) * (integrate g)
    operations maintain ordering, if present
-}

{-----------------------------------------------------------------------------
    Class instances for polynomials
------------------------------------------------------------------------------}
instance (Eq a, Num a, Fractional a) => Evaluable a (Poly a) where
    evaluate x p = eval p x
    boost :: (Eq a, Num a, Fractional a) => a -> Poly a -> Poly a
    boost x y = y + constant x
    scale = Poly.scale

instance (Fractional a, Eq a, Ord a) => Comparable a (Poly a) where
    compareObjects (lf, uf, (f, g)) = compareToZero (lf, uf, f - g)

instance (Num a, Eq a, Fractional a) => Mergeable (Poly a) where
    mergeObject x y = if x == y then Just y else Nothing
    zero = Poly.zero

instance (Ord a, Num a, Eq a, Fractional a) => Displayable a (Poly a) where
    displayObject s (l, u, p) =
        if l >= u
            then error "Invalid polynomial interval"
            else Right (display p (l, u) s)

instance (Ord a, Num a, Eq a, Fractional a) => ComplexityMeasureable (Poly a) where
    measureComplexity x = if degree x <= 0 then 1 else degree x
