{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{-|
Copyright   : Predictable Network Solutions Ltd., 2024
License     : BSD-3-Clause
Maintainer  : peter.thompson@pnsol.com
Description : Tests for polynomials.
-}
module Numeric.Polynomial.SimpleSpec
    ( spec
    ) where

import Prelude

import Numeric.Polynomial.Simple
    ( Poly
    , constant
    , degree
    , differentiate
    , eval
    , fromCoefficients
    , integrate
    , lineFromTo
    , scale
    , scaleX
    , translate
    )
import Test.Hspec
    ( Spec
    , describe
    , it
    , pendingWith
    , xit
    )
import Test.QuickCheck
    ( Arbitrary
    , (===)
    , (==>)
    , (.&&.)
    , arbitrary
    , counterexample
    , listOf
    , property
    )

{-----------------------------------------------------------------------------
    Tests
------------------------------------------------------------------------------}
spec :: Spec
spec = do
    describe "constant" $ do
        it "eval" $ property $
            \c (x :: Rational) ->
                eval (constant c) x  ===  c 

    describe "scale" $ do
        it "eval" $ property $
            \a p (x :: Rational) ->
                eval (scale a p) x  ===  a * eval p x

    describe "scaleX" $ do
        it "degree" $ property $
            \(p :: Poly Rational) ->
                (degree p >= 0)
                ==> (degree (scaleX p) === 1 + degree p)

        it "eval" $ property $
            \p (x :: Rational) ->
                eval (scaleX p) x  ===  x * eval p x

    describe "(+)" $ do
        it "eval" $ property $
            \p q (x :: Rational) ->
                eval (p + q) x  ===  eval p x + eval q x

    describe "(*)" $ do
        it "eval" $ property $
            \p q (x :: Rational) ->
                eval (p * q) x  ===  eval p x * eval q x

    describe "lineFromTo" $ do
        it "degree" $ property $
            \x1 (x2 :: Rational) y1 y2 ->
                let p = lineFromTo (x1, y1) (x2, y2)
                in  x1 /= x2
                    ==> degree p <= 1

        it "eval" $ property $
            \x1 (x2 :: Rational) y1 y2 ->
                let p = lineFromTo (x1, y1) (x2, y2)
                in  x1 /= x2
                    ==> (eval p x1 === y1  .&&.  eval p x2 == y2)


    describe "integrate" $ do
        it "eval" $ property $
            \(p :: Poly Rational) ->
                eval (integrate p) 0  ===  0

        it "integrate . differentiate" $ property $
            \(p :: Poly Rational) ->
                integrate (differentiate p) ===  p - constant (eval p 0)

    describe "differentiate" $ do
        it "differentiate . integrate" $ property $
            \(p :: Poly Rational) ->
                differentiate (integrate p)  ===  p

        it "Leibniz rule" $ property $
            \(p :: Poly Rational) q ->
                differentiate (p * q)
                    ===  (differentiate p) * q + p * (differentiate q)

    describe "translate" $ do
        it "…" $ do
            pendingWith
                $ "Failures for degree > 70, probably Int overflow "
                <> "when computing binomial coefficients."

        xit "eval" $ property $
            \p y (x :: Rational) ->
                counterexample ("degree p = " <> show (degree p))
                $ eval (translate y p) x  ===  eval p (x - y)

        xit "differentiate" $ property $
            \p (y :: Rational) ->
                counterexample ("degree p = " <> show (degree p))
                $ differentiate (translate y p)
                    ===  translate y (differentiate p)

{-----------------------------------------------------------------------------
    Random generators
------------------------------------------------------------------------------}
instance Arbitrary (Poly Rational) where
    arbitrary = fromCoefficients <$> listOf arbitrary
