{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}

{-|
Module      : Deltas
Description : Polynomials extended with delta functions
Copyright   : (c) Peter Thompson, 2024
License     : BSD-2-Clause
Maintainer  : peter.thompson@pnsol.com
Stability   : experimental

We extend the polynomials with delta functions so that we can have 'pieces' of either.
We can add and multiply polynomials and add deltas; multiplying deltas only makes sense
in the context of multiplying CDFs, when it will be OK.
We will only be combining a delta and a poly over a zero interval, where the delta dominates.
We can define the operators in a way that keeps the deltas intact and makes the
piecewise integration/differentiation work.
-}
module PWPs.PolyDeltas
(
      PolyDelta (..)
)
where
import PWPs.PiecewiseClasses
import PWPs.SimplePolynomials as SP

{- |
A PolyDelta is either a polynomial or a (shifted, scaled) Delta with a mass. 
The position of a Delta is stored as its basepoint when doing piecewise operations.
-}
data PolyDelta a = Pd (Poly a) | D a
    deriving (Show)
instance Eq a => Eq (PolyDelta a)
    where
        Pd x == Pd y = x == y
        D x  == D y  = x == y
        Pd _ == D _  = False
        D _  == Pd _ = False
instance Functor PolyDelta where
    fmap f (D x) = D (f x)
    fmap f (Pd x) = Pd (fmap f x)

type MyConstraints a = (Eq a, Num a, Fractional a)
type EqNum a = (Eq a, Num a)
type OrdNumEqFrac a = (Ord a, Num a, Eq a, Fractional a)

plusPD :: (Eq a, Fractional a) => PolyDelta a -> PolyDelta a -> PolyDelta a
-- Polynomials have zero mass at a single point, so they are dominated by Ds and Hs
plusPD (Pd x) (Pd y) = Pd (x + y)
plusPD (Pd _) (D x)  = D x
plusPD (D x) (Pd _)  = D x
plusPD (D x) (D x')  = D (x + x')

timesPD :: (Eq a, Fractional a) => PolyDelta a -> PolyDelta a -> PolyDelta a
timesPD (Pd x) (Pd y) = Pd (x * y)
timesPD (Pd _) (D x)  = D x
timesPD (D x) (Pd _)  = D x
timesPD (D x) (D x')  = D (x * x')

instance MyConstraints a => Num (PolyDelta a) where
    (+)           = plusPD
    (*)           = timesPD
    negate        = fmap negate
    abs           = undefined
    signum        = undefined
    fromInteger n = Pd $ makePoly $ Prelude.fromInteger n

scalePD :: EqNum a => a -> PolyDelta a -> PolyDelta a
scalePD x (Pd a) = Pd (SP.scalePoly x a)
scalePD x (D y)  = D (x * y)

evaluatePD :: EqNum a => a -> PolyDelta a -> [a]
evaluatePD point (Pd x) = [SP.evaluatePoly point x]
evaluatePD _ (D x)      = [x]

boostPD :: MyConstraints a => a -> PolyDelta a -> PolyDelta a
boostPD x (Pd y) = Pd y + Pd (makePoly x)
boostPD _ (D y)  = D y
instance MyConstraints a => Evaluable a (PolyDelta a)
    where
        evaluate  = evaluatePD
        boost     = boostPD
        scale     = scalePD

-- | Removes excess basepoints if the objects on either side are the same
aggregate :: Eq a => [(a, PolyDelta a)] -> [(a, PolyDelta a)]
aggregate []    = error "Empty list of polydeltas"
aggregate [x]   = [x] -- need at least two elements to do anything
aggregate ((bx, x):ys@((_, y):xs))
    | x == y    = aggregate ((bx, x):xs) -- throw away the second basepoint
    | otherwise = (bx, x) : aggregate ys

convolvePolyDeltas :: (Num a, Fractional a, Ord a)
                   => (a, a, PolyDelta a) -> (a, a, PolyDelta a) -> [(a, PolyDelta a)]
{- |
When both arguments are polynomials, we check the intervals are non-zero then use convolvePolys and just map the type.
For a delta, lower == upper (invariant to be checked), and the effect of the delta is to translate the other
argument (whichever it is) along by this amount. Need to ensure there is still an initial interval based at zero.
-}
convolvePolyDeltas (lf, uf, Pd f) (lg, ug, Pd g)
    | (uf <= lf) || (ug <= lg) = error "Invalid polynomial interval width"
    -- convolve the polynomials to get a list of intervals, put the type back and remove redundant intervals
    | otherwise = aggregate $ map (\(x, p) -> (x, Pd p)) (convolvePolys (lf, uf, f) (lg, ug, g))
convolvePolyDeltas (lf, uf, D f) (lg, ug, Pd g)
    | lf /= uf     = error "Non-zero delta interval"
    | ug < lg      = error "Negative interval width"
    -- convolving with a zero-sized delta gives nothing
    | f == 0       = [(0, Pd zeroPoly)]
    -- degenerate case of delta at zero: don't shift but scale by the mass of the delta
    | lf == 0      = [(lg, scalePD f (Pd g)), (ug, Pd zeroPoly)]

    | otherwise    = aggregate [(0, Pd zeroPoly), (lg + lf, scalePD f (Pd (shiftPoly lf g))), (ug + lf, Pd zeroPoly)]
convolvePolyDeltas (lf, uf, Pd f) (lg, ug, D g) = convolvePolyDeltas (lg, ug, D g) (lf, uf, Pd f)  -- commutative
convolvePolyDeltas (lf, uf, D f) (lg, ug, D g) -- both deltas
    | lf /= uf || lg /= ug  = error "Non-zero delta interval"
    -- convolving with a zero-sized delta gives nothing
    | f * g == 0            = [(0, Pd zeroPoly)]
    -- degenerate case of deltas at zero: no shifting but mutiply the masses
    | lg + lf == 0          = [(0, D (f * g)), (0, Pd zeroPoly)]
    -- Shift by the sum of the basepoints, multiply the masses, and insert a new initial zero interval
    | otherwise             = [(0, Pd zeroPoly), (lg + lf, D (f * g)), (lg + lf, Pd zeroPoly)]

instance (Num a, Fractional a, Ord a) => CompactConvolvable a (PolyDelta a)
    where
        convolveIntervals = convolvePolyDeltas

{-|
    We merge polynomials if they are equal. We merge Ds/Hs by adding them (not that we expect this case).
    We merge a zero D/H with a polynomial by discarding it. Other cases do not merge.
-}
instance (Num a, Eq a, Fractional a) => Mergeable (PolyDelta a)
    where
        mergeObject a b = case (a, b) of
            (Pd x, Pd y) -> if x == y then Just (Pd y) else Nothing
            (D x, Pd y)  -> if x == 0 then Just (Pd y) else Nothing
            (D x, D y)   -> Just (D (x + y))
            (_, _)       -> Nothing
        zero = Pd zeroPoly

displayPolyDelta :: OrdNumEqFrac a => a -> (a, a, PolyDelta a) -> Either (a,a) [(a, a)]
displayPolyDelta _ (l, u, D x)
    | l /= u    = error "Non-zero delta interval"
    | otherwise = Left (l, x)
displayPolyDelta s (l, u, Pd p)
    | l >= u    = error "Invalid polynomial interval"
    | otherwise = Right (displayPoly p (l, u) s)
instance OrdNumEqFrac a => Displayable a (PolyDelta a)
    where
        displayObject = displayPolyDelta

instance OrdNumEqFrac a => ComplexityMeasureable (PolyDelta a)
    where
        measureComplexity (Pd (Poly a)) = if SP.degreePoly (Poly a) <= 0 then 1 else SP.degreePoly (Poly a)
        measureComplexity (D _) = 1
