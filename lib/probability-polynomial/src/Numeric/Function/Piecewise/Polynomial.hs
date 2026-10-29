{-|
Copyright   : Predictable Network Solutions Ltd., 2020-2026
License     : BSD-3-Clause
Description : Functions specific to piecewise polynomials.
-}
module Numeric.Function.Piecewise.Polynomial
    ( -- * Operations
      integral
    ) where

import Data.List
    ( mapAccumL
    )
import Numeric.Function.Piecewise
    ( Piecewise
    , fromAscPieces
    , toAscPieces
    )
import Numeric.Polynomial.Simple
    ( Poly
    )

import qualified Numeric.Polynomial.Simple as Poly

{------------------------------------------------------------------------------
    Operations
    Integration
------------------------------------------------------------------------------}
{- | The continuous antiderivative of a piecewise polynomial,
     taking the value 0 at the start of the first piece.
-}
integral
    :: (Ord a, Fractional a)
    => Piecewise (Poly a) -> Piecewise (Poly a)
integral = fromAscPieces . snd . mapAccumL f Poly.zero . toAscPieces
  where
    -- The accumulator is the antiderivative on the previous piece.
    -- Shift the antiderivative on this piece so that it starts
    -- where the previous one ends.
    f before (x, p) = (q, (x, q))
      where
        pp = Poly.integrate p
        q = pp + Poly.constant (Poly.eval before x - Poly.eval pp x)
