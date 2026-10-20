{-# LANGUAGE ScopedTypeVariables #-}

{-|
Copyright   : Predictable Network Solutions Ltd., 2020-2026
License     : BSD-3-Clause
-}
module Numeric.Function.Piecewise.PolynomialSpec
    ( spec
    , genPiecewisePoly
    ) where

import Prelude

import Data.Function.Class
    ( eval
    )
import Numeric.Function.Piecewise
    ( Piecewise
    , mapPieces
    , toAscPieces
    )
import Numeric.Function.Piecewise.Polynomial
    ( integral
    )
import Numeric.Function.PiecewiseSpec
    ( genPiecewise
    )
import Numeric.Polynomial.Simple
    ( Poly
    , differentiate
    , scale
    )
import Numeric.Polynomial.SimpleSpec
    ( genPoly
    )
import Test.Hspec
    ( Spec
    , describe
    , it
    )
import Test.QuickCheck
    ( Gen
    , (===)
    , property
    )

{-----------------------------------------------------------------------------
    Tests
------------------------------------------------------------------------------}
spec :: Spec
spec = do
    describe "integral" $ do
        it "is continuous" $ property $
            \(p :: Piecewise (Poly Rational)) ->
                let pieces = toAscPieces (integral p)
                    befores = 0 : map snd pieces
                in  zipWith (\b (x, _) -> eval b x) befores pieces
                        === map (\(x, q) -> eval q x) pieces

        it "is an antiderivative" $ property $
            \(p :: Piecewise (Poly Rational)) x ->
                eval (mapPieces differentiate (integral p)) x
                    === eval p x

        it "is additive" $ property $
            \p (q :: Piecewise (Poly Rational)) x ->
                eval (integral (p + q)) x
                    === eval (integral p + integral q) x

        it "is homogeneous" $ property $
            \(a :: Rational) (p :: Piecewise (Poly Rational)) x ->
                eval (integral (mapPieces (scale a) p)) x
                    === a * eval (integral p) x

{-----------------------------------------------------------------------------
    Random generators
------------------------------------------------------------------------------}
-- | Generate an arbitrary piecewise polynomial,
-- with sorted breakpoints and an arbitrary polynomial on each piece.
genPiecewisePoly :: Gen (Piecewise (Poly Rational))
genPiecewisePoly = genPiecewise genPoly
