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
    , convolve
    , countRoots
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
    , NonNegative (..)
    , Positive (..)
    , (===)
    , (==>)
    , (.&&.)
    , arbitrary
    , counterexample
    , listOf
    , mapSize
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
                    ===  differentiate p * q + p * differentiate q

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

    describe "convolve" $ do
        it "product of integrals" $ property $ mapSize (`div` 6) $
            \(NonNegative x1) (Positive d1) (NonNegative x2) (Positive d2)
              p (q :: Poly Rational) ->
                let p1 = (x1, x1 + d1, p)
                    q1 = (x2, x2 + d2, q)
                in
                    integrateInterval p1 * integrateInterval q1
                        === integratePieces (convolve p1 q1)

    describe "countRoots" $ do
        it "disjoint roots and interval" $ property $ mapSize (`div` 5) $
            \(DisjointSorted roots) x (Positive d) ->
                let xx = scaleX (constant 1) :: Poly Rational
                    -- Vieta's formula
                    p = product $ map (\r -> xx - constant r) roots
                    y = x + d
                in
                    (x `notElem` roots) && (y `notElem` roots)
                    ==> (countRoots (x, y, p)
                        ===  countIntervalMembers (x, y) roots)

{-----------------------------------------------------------------------------
    Helper functions
------------------------------------------------------------------------------}
-- | Definite integral of a polynomial over an interval.
integrateInterval
    :: (Eq a, Num a, Fractional a) => (a, a, Poly a) -> a
integrateInterval (x, y, p) = eval pp y - eval pp x
  where pp = integrate p

-- | Definite integral of a sequence of polynomials over pieces.
integratePieces
    :: (Eq a, Num a, Fractional a) => [(a, Poly a)] -> a
integratePieces = sum . map integrateInterval . intervals
  where
    intervals pieces =
        [ (x, y, p)
        | ((x, p), y) <- zip pieces $ drop 1 $ map fst pieces
        ]

-- | Count the number of list elements that fall in a given interval.
countIntervalMembers :: Ord a => (a, a) -> [a] -> Int 
countIntervalMembers (xl, xr) =
    length . filter (\x -> xl < x && x <= xr)

{-----------------------------------------------------------------------------
    Random generators
------------------------------------------------------------------------------}
instance Arbitrary (Poly Rational) where
    arbitrary = fromCoefficients <$> listOf arbitrary

-- | A list of disjoint and sorted elements.
newtype DisjointSorted a = DisjointSorted [a]
    deriving (Eq, Show)

instance Arbitrary (DisjointSorted Rational) where
    arbitrary =
        DisjointSorted . drop 1 . scanl (\s (Positive d) -> s + d) 0
            <$> listOf arbitrary