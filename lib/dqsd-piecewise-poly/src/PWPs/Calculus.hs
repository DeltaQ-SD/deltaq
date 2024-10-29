{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-|
Module      : Calculus
Description : Integration and differentiation of extended polynomials
Copyright   : (c) Peter Thompson, 2024
License     : BSD-2-Clause
Maintainer  : peter.thompson@pnsol.com
Stability   : experimental

We convert between polynomials with delta functions or Heavides using integration and differentiation.
-}
module PWPs.Calculus
    (
    )
where

import PWPs.PiecewiseClasses
import PWPs.PolyDeltas as PD
import PWPs.PolyHeavisides as PH
import Numeric.Polynomial.Simple as SP

type MyConstraints a = (Eq a, Num a, Fractional a)

-- | We integrate PolyDeltas to get PolyHeavisides
integratePD :: (Eq a, Fractional a) => PolyDelta a -> PolyHeaviside a
integratePD (Pd x) = Ph (integratePoly x)
integratePD (D x) = H 0 x

-- | We differentiate PolyHeavisides to get PolyDeltas
differentiatePH :: MyConstraints a => PolyHeaviside a -> PolyDelta a
differentiatePH (Ph x) = Pd (differentiatePoly x)
differentiatePH (H x y) = D (y - x)

instance MyConstraints a => Integrable (PolyDelta a) (PolyHeaviside a) where
    integrate = integratePD

instance MyConstraints a => Differentiable (PolyHeaviside a) (PolyDelta a) where
    differentiate = differentiatePH
