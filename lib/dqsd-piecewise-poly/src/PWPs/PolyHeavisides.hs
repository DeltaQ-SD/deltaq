{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}

{-|
Module      : Heavisides
Description : Polynomials extended with Heaviside step functions
Copyright   : (c) Peter Thompson, 2024
License     : BSD-2-Clause
Maintainer  : peter.thompson@pnsol.com
Stability   : experimental

We extend the polynomials with step functions so that we can have 'pieces' of either.
We can add and multiply polynomials and add steps; multiplying steps only makes sense
in the context of multiplying CDFs, when it will be OK.
We will only be combining a step and a poly over a zero interval, where the step dominates.
-}
module PWPs.PolyHeavisides
(
      PolyHeaviside (..)
    , polyHeavisideRoot
)
where
import PWPs.PiecewiseClasses
import PWPs.SimplePolynomials as SP

{- |
A PolyHeaviside is either a polynomial or a (shifted, scaled) Heaviside. 
A Heaviside has a starting value and a rise; 
for probabilities all should be constrained between 0 and 1. 
The position of a Heavisides is stored as its basepoint when doing piecewise operations.
-}

data PolyHeaviside a = Ph (Poly a) | H a a
    deriving (Show)

instance Eq a => Eq (PolyHeaviside a)
    where
        Ph x  == Ph y = x == y
        H x y == H x' y'  = (x == x') && (y == y')
        Ph _  == H _ _  = False
        H _ _ == Ph _ = False

instance Functor PolyHeaviside where
    fmap f (H x y) = H (f x) (f y)
    fmap f (Ph x) = Ph (fmap f x)

type MyConstraints a = (Eq a, Num a, Fractional a)
type EqNum a = (Eq a, Num a)
type OrdNumEqFrac a = (Ord a, Num a, Eq a, Fractional a)

plusPH :: (Eq a, Fractional a) => PolyHeaviside a -> PolyHeaviside a -> PolyHeaviside a
-- Polynomials have zero mass at a single point, so they are dominated by Ds and Hs
plusPH (Ph x) (Ph y)     = Ph (x + y)
plusPH (Ph _) (H x y)    = H x y
plusPH (H x y) (Ph _)    = H x y
plusPH (H x y) (H x' y') = H (x + x') (y + y')

timesPH :: (Eq a, Fractional a) => PolyHeaviside a -> PolyHeaviside a -> PolyHeaviside a
timesPH (Ph x) (Ph y)     = Ph (x * y)
timesPH (Ph _) (H x y)    = H x y
timesPH (H x y) (Ph _)    = H x y
timesPH (H x y) (H x' y') = H (x * x') (y * y')
instance MyConstraints a => Num (PolyHeaviside a) where
    (+)           = plusPH
    (*)           = timesPH
    negate        = fmap negate
    abs           = undefined
    signum        = undefined
    fromInteger n = Ph $ makePoly $ Prelude.fromInteger n

scalePH :: EqNum a => a -> PolyHeaviside a -> PolyHeaviside a
scalePH x (Ph a)  = Ph (SP.scalePoly x a)
scalePH x (H y z) = H (x * y) (x * z)

evaluatePH :: EqNum a => a -> PolyHeaviside a -> [a]
evaluatePH point (Ph x) = [SP.evaluatePoly point x]
evaluatePH _ (H x y)    = [x, y]

boostPH :: MyConstraints a => a -> PolyHeaviside a -> PolyHeaviside a
boostPH x (Ph y) = Ph y + Ph (makePoly x)
boostPH x (H y z) = H (x + y) (x + z)

instance MyConstraints a => Evaluable a (PolyHeaviside a)
    where
        evaluate  = evaluatePH
        boost     = boostPH
        scale     = scalePH

{-|
    We measure whether a polyheaviside is consistently above or below another, or equals 
-}
comparePHs :: (Fractional a, Eq a, Ord a) => (a, a, (PolyHeaviside a, PolyHeaviside a)) -> Maybe Ordering
-- simple polynomial case: f >= g <=> f - g >= 0
comparePHs (lf, uf, (Ph f, Ph g)) = SP.compareToZero (lf, uf, f - g) 
-- compare a Heaviside step to a polynomial at a point
comparePHs (lf, uf, (H x y, Ph f))
    | lf /= uf                                           = error "Non-zero Heaviside interval"
    -- they can be equal only if the Heaviside step is 0
    | (x == fx) && (y == fx)                             = Just EQ
    -- if the bottom of the Heaviside surpasses the polynomial it is greater, given its height is non-zero
    | x >= fx                                            = Just GT
    -- if the top of the Heaviside is surpassed by the polynomial, it is lesser
    | y <= fx                                            = Just LT
    | otherwise                                          = Nothing
        where
            fx = evaluatePoly lf f
-- if the Heaviside and the polynomial are the other way round, swap them and reverse the ordering
comparePHs (lf, uf, (Ph f, H x y)) = reverseOrder $ comparePHs (lf, uf, (H x y, Ph f))
    where
        reverseOrder o = case o of
            Nothing -> Nothing
            Just EQ -> Just EQ
            Just LT -> Just GT
            Just GT -> Just LT
-- Comparing two Heavisides at the same point requires comparing their bases and steps
comparePHs (lf, uf, (H x y, H x' y'))
    | lf /= uf                                           = error "Non-zero Heaviside interval"
    | (x == x') && (y == y')                             = Just EQ
    | ((x > x') && (y >= y')) || ((x >= x') && (y > y')) = Just GT
    | ((x < x') && (y <= y')) || ((x <= x') && (y < y')) = Just LT
    | otherwise                                          = Nothing
instance (Fractional a, Eq a, Ord a) => Comparable a (PolyHeaviside a)
    where
        compareObjects = comparePHs

{-|
    We merge polynomials if they are equal. We merge Heavisides by adding them (not that we expect this case).
    We merge a zero H with a polynomial by discarding it. Other cases do not merge.
-}

instance (Num a, Eq a, Fractional a) => Mergeable (PolyHeaviside a)
    where
        mergeObject a b = case (a, b) of
            -- merge polynomials iff they are equal
            (Ph x, Ph y)     -> if x == y then Just (Ph y) else Nothing
            -- merge a Heaviside with a following polynomial only if the step is zero
            (H x y, Ph z)    -> if x == y then Just (Ph z) else Nothing
            -- Merge two Heavisides if they stack correctly
            (H x y, H x' y') -> if y == x' then Just (H x y') else Nothing
            (_, _)           -> Nothing
        zero = Ph zeroPoly

{-|
    Given an interval containing a given value of a PolyHeaviside, find its location
-}
polyHeavisideRoot  :: OrdNumEqFrac a => a -> a -> (a, a) -> PolyHeaviside a -> Maybe a
-- If we have a step, the interval is zero width so this is the root
polyHeavisideRoot  _ _ (l, u) (H _ _) = if l /= u then error "Non-zero Heaviside interval" else Just l
-- otherwise we have a polynomial: subtract the value we are looking for so that we seek a zero crossing
polyHeavisideRoot  e x (l, u) (Ph p) = findPolyRoot e (l, u) (p - makePoly x)

displayPolyHeaviside  :: OrdNumEqFrac a => a -> (a, a, PolyHeaviside a) -> Either (a,a) [(a, a)]
displayPolyHeaviside  s (l, u, Ph p)  = if l >= u then error "Invalid polynomial interval"
                                    else Right (displayPoly p (l, u) s)
displayPolyHeaviside  _ (l, u, H x _) = if l /= u then error "Non-zero heaviside interval"
                                    else Left (l, x)

instance OrdNumEqFrac a => Displayable a (PolyHeaviside a)
    where
        displayObject = displayPolyHeaviside

instance OrdNumEqFrac a => ComplexityMeasureable (PolyHeaviside a)
    where
        measureComplexity (Ph (Poly a)) = if SP.degreePoly (Poly a) <= 0 then 1 else SP.degreePoly (Poly a)
        measureComplexity (H _ _) = 1