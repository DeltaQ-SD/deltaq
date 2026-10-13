
{-|
Copyright   : Predictable Network Solutions Ltd., 2020-2026
License     : BSD-3-Clause
Description : Antiderivative of piecewise polynomials.

Compute the integral of a mixed discrete / continuous function
where the continuous part is represented in terms of piecewise polynomials.
-}
module Numeric.Function.Piecewise.Polynomial
    ( 
    -- * Operations
    integral
    ) where
import Numeric.Function.Piecewise
    ( Piecewise
    , toAscPieces
    , fromAscPieces
    )
import qualified Numeric.Polynomial as Poly
{------------------------------------------------------------------------------
    Operations
    Integration
------------------------------------------------------------------------------}
{- | The continuous antiderivative of a piecewise polynomial, 
     taking the value 0 at the start of the first piece.
-}
integral :: (Num a, Ord a) => Piecewise (Poly a) -> Piecewise (Poly a)
integral = fromAscPieces . scanl f 0 . toAscPieces
    where
        f c (x, p) = (x, q)
            where
                pp = Poly.integrate p
                q = pp + constant (c - Poly.eval pp x) -- shift so this piece starts at the accumulated value c