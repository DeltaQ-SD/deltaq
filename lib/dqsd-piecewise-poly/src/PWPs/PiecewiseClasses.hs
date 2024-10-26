{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
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
(
    Integrable (..)
  , Differentiable (..)
  , Evaluable (..)
  , CompactConvolvable (..)
  , Comparable (..)
  , Mergeable (..)
  , Displayable (..)
  , ComplexityMeasureable (..)
)
where

class Integrable a b where
    integrate     :: a -> b

class Differentiable a b where
    differentiate :: a -> b

class Evaluable a b where
    evaluate :: a -> b -> [a] -- evaluate b at point a
    boost    :: a -> b -> b -- increment b by a
    scale    :: a -> b -> b -- scale b by a

{- |
    Convolution in our library is over finite intervals - this is what piecewiseness needs
-}
class CompactConvolvable a b where
    convolveIntervals :: (a, a, b) -> (a, a, b) -> [(a, b)]  -- convolution

{- |
    We express a partial order by comparing a pair of objects on an interval, delivering a Maybe Ordering 
-}
class Comparable a b where
    compareObjects :: (a, a, (b, b)) -> Maybe Ordering

{- |
    We want to know when two objects can be merged - this is not the same as saying they are equal
    We also define a zeroObject to be used when there aren't two objects to be merged
-}
class Mergeable a where
    mergeObject :: a -> a -> Maybe a
    zero        :: a

class Displayable a b where
    displayObject :: a -> (a, a, b) -> Either (a,a) [(a, a)]

class ComplexityMeasureable a where
    measureComplexity :: a -> Int
    
{- |
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
