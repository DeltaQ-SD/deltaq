{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

{-|
Copyright   : Predictable Network Solutions Ltd., 2024
License     : BSD-3-Clause
Maintainer  : peter.thompson@pnsol.com
-}
module Numeric.Polynomial.SimpleSpec
    ( spec
    , genPoly
    , genPositivePoly
    ) where

import Prelude

import Data.List
    ( nub
    )
import Numeric.Polynomial.Simple
    ( Poly
    , compareToZero
    , constant
    , convolve
    , countRoots
    , degree
    , differentiate
    , display
    , eval
    , fromCoefficients
    , integrate
    , isMonotonicallyIncreasingOn
    , lineFromTo
    , root
    , scale
    , scaleX
    , translate
    , zero
    )
import Test.Hspec
    ( Spec
    , before_
    , describe
    , it
    , pendingWith
    )
import Test.QuickCheck
    ( Arbitrary
    , Gen
    , NonNegative (..)
    , Positive (..)
    , Property
    , (===)
    , (==>)
    , (.&&.)
    , arbitrary
    , counterexample
    , forAll
    , listOf
    , mapSize
    , property
    , withMaxSuccess
    )

import qualified Test.QuickCheck as QC

{-----------------------------------------------------------------------------
    Tests
------------------------------------------------------------------------------}
xit' :: String -> String -> Property -> Spec
xit' reason label = before_ (pendingWith reason) . it label

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

        it "zero" $ withMaxSuccess 1 $ property $
            scaleX zero  ==  (zero :: Poly Rational)

    describe "(+)" $ do
        it "eval" $ property $
            \p q (x :: Rational) ->
                eval (p + q) x  ===  eval p x + eval q x

    describe "(*)" $ do
        it "eval" $ property $
            \p q (x :: Rational) ->
                eval (p * q) x  ===  eval p x * eval q x

    describe "display" $ do
        it "step == 0" $ property $
            \(l :: Rational) (Positive d) ->
                let u = l + d
                in  display zero (l, u) 0
                        === zip [l, u] (repeat 0)

        it "zero" $ property $
            \(l :: Rational) (Positive d) (Positive (n :: Integer)) ->
                let u = l + d
                    s = (u - l) / fromIntegral (min 100 n)
                in  display zero (l, u) s
                        === zip (nub ([l, l+s .. u] <> [u])) (repeat 0)

    describe "lineFromTo" $ do
        it "degree" $ property $
            \x1 (x2 :: Rational) y1 y2 ->
                let p = lineFromTo (x1, y1) (x2, y2)
                in  degree p <= 1

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
        it "eval" $ property $
            \p y (x :: Rational) ->
                counterexample ("degree p = " <> show (degree p))
                $ eval (translate y p) x  ===  eval p (x - y)

        it "differentiate" $ property $
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

    describe "root" $ do
        it "cubic polynomial" $ property $ mapSize (`div` 5) $
            \(x1 :: Rational) (Positive dx3) ->
                let xx = scaleX (constant 1) :: Poly Rational
                    x2 = 0.6 * x1 + 0.4 * x3
                    x3 = x1 + dx3
                    p = (xx - constant x1) * (xx - constant x2) * (xx - constant x3)
                    l = x1 + 100 * epsilon
                    u = x3 - 100 * epsilon
                    epsilon = (x3-x1)/(1000*1000*50)
                    Just x2' = root epsilon 0 (l, u) p
                in
                    property $ abs (x2' - x2) <= epsilon

        xit' "bug" "cubic polynomial, midpoint" $ property $ mapSize (`div` 5) $
            \(x1 :: Rational) (Positive dx3) ->
                let xx = scaleX (constant 1) :: Poly Rational
                    x2 = (x1 + x3) / 2
                    x3 = x1 + dx3
                    p = (xx - constant x1) * (xx - constant x2) * (xx - constant x3)
                    l = x1 + 100 * epsilon
                    u = x3 - 100 * epsilon
                    epsilon = (x3-x1)/(1000*1000*50)
                    Just x2' = root epsilon 0 (l, u) p
                in
                    id
                    $ counterexample ("interval = " <> show (l,u))
                    $ counterexample ("countRoots = " <> show (countRoots (l, u, p)))
                    $ counterexample ("expected root = " <> show x2)
                    $ counterexample ("eval polynomial at expected root = " <> show (eval p x2))
                    $ counterexample ("epsilon = " <> show epsilon)
                    $ counterexample ("found root = " <> show x2')
                    $ counterexample ("root within range of other root " <> show (abs (x2' - x3) <= 20*epsilon))
                    $ property $ abs (x2' - x2) <= epsilon

    describe "isMonotonicallyIncreasingOn" $
        it "quadratic polynomial" $ property $
            \(x1 :: Rational) (Positive d) ->
                let xx = scaleX (constant 1)
                    p  = negate ((xx - constant x1) * (xx - constant x2))
                    x2 = x1 + d
                    xmid = (x1 + x2) / 2
                in
                    isMonotonicallyIncreasingOn p (x1,xmid)  ===  True

    describe "compareToZero" $ do
        it "lineFromTo" $ property $
            \(x1 :: Rational) (Positive dx) y1 (Positive dy) ->
                let x2 = x1 + dx
                    y2 = y1 + dy
                    p = lineFromTo (x1, y1) (x2, y2)
                    result
                        | y1 == 0 && y2 == 0 = Just EQ
                        | y1 >= 0 = Just GT
                        | y2 <= 0 = Just LT
                        | otherwise = Nothing
                in
                    compareToZero (x1, x2, p)
                        === result

        it "quadratic polynomial with two roots" $ property $
            \(x1 :: Rational) (Positive d) ->
                let xx = scaleX (constant 1)
                    p  = (xx - constant x1 + 1) * (xx - constant x2 - 1)
                    x2 = x1 + d
                in
                    compareToZero (x1, x2, p)  ===  Just LT

        it "quadratic polynomial + a0" $ property $
            \(x1 :: Rational) a0 ->
                let xx = scaleX (constant 1)
                    p  = (xx - constant x1)^(2 :: Int) + constant a0
                in
                    compareToZero (x1 - abs a0 - 1, x1 + abs a0 + 1, p)
                        === 
                        if a0 > 0
                            then Just GT
                            else Nothing

    describe "genPositivePoly" $
        it "eval" $ property $
            \(x :: Rational) ->
            forAll genPositivePoly $ \p ->
                eval p x >= 0

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
genPoly :: Gen (Poly Rational)
genPoly = fromCoefficients <$> listOf arbitrary

-- | Generate a positive polynomial, i.e. @eval p x >= 0@ for all @x@.
genPositivePoly :: Gen (Poly Rational)
genPositivePoly =
    QC.scale (`div` 3) $ product <$> listOf genQuadratic
  where
    xx = fromCoefficients [0, 1]

    genQuadratic = do
        x0 <- constant <$> arbitrary
        NonNegative b <- arbitrary
        pure $ (xx - x0) * (xx - x0) + constant b

instance Arbitrary (Poly Rational) where
    arbitrary = genPoly

-- | A list of disjoint and sorted elements.
newtype DisjointSorted a = DisjointSorted [a]
    deriving (Eq, Show)

instance Arbitrary (DisjointSorted Rational) where
    arbitrary =
        DisjointSorted . drop 1 . scanl (\s (Positive d) -> s + d) 0
            <$> listOf arbitrary