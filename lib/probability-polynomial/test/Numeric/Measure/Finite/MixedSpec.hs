{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{-|
Copyright   : Predictable Network Solutions Ltd., 2024
License     : BSD-3-Clause
Maintainer  : peter.thompson@pnsol.com
Description : Tests for finite signed measures.
-}
module Numeric.Measure.Finite.MixedSpec
    ( spec
    ) where

import Prelude

import Data.Function.Class
    ( eval
    )
import Data.Maybe
    ( fromJust
    )
import Numeric.Measure.Finite.Mixed
    ( Measure
    , add
    , convolve
    , dirac
    , distribution
    , fromDistribution
    , scale
    , support
    , total
    , translate
    , uniform
    , zero
    )
import Numeric.Function.PiecewiseSpec
    ( genPiecewise
    )
import Numeric.Polynomial.SimpleSpec
    ( genPoly
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
    , Gen
    , (===)
    , (==>)
    , arbitrary
    , cover
    , mapSize
    , property
    )

import qualified Numeric.Function.Piecewise as Piecewise
import qualified Numeric.Polynomial.Simple as Poly

{-----------------------------------------------------------------------------
    Tests
------------------------------------------------------------------------------}
spec :: Spec
spec = do
    describe "dirac" $ do
        it "total" $ property $
            \(x :: Rational) ->
                total (dirac x)  ===  1

    describe "uniform" $ do
        it "total" $ property $
            \(x :: Rational) y ->
                total (uniform x y)  ===  1

        it "support" $ property $
            \(x :: Rational) y ->
                support (uniform x y)  ===  Just (min x y, max x y)

        it "distribution at midpoint" $ property $
            \(x :: Rational) (y :: Rational) ->
                x /= y ==>
                eval (distribution (uniform x y)) ((x + y) / 2)  ===  1/2

    describe "instance Eq" $ do
        it "add m (scale (-1) m) == zero" $ property $
            \(m :: Measure Rational) ->
                cover 80 (total m /= 0) "nontrivial"
                $ add m (scale (-1) m)  ===  zero
        
        it "dirac x /= dirac y" $ property $
            \(x :: Rational) (y :: Rational) ->
                x /= y  ==>  dirac x /= dirac y

    describe "add" $ do
        it "total" $ property $
            \(mx :: Measure Rational) my ->
                total (add mx my)  ===  total mx + total my

    describe "translate" $ do
        it "…" $ do
            pendingWith "Failures in Poly.translate"

        xit "distribution" $ property $
            \(m :: Measure Rational) y x ->
                eval (distribution (translate y m)) x
                    ===  eval (distribution m) (x - y)

    describe "convolve" $ do
        it "dirac dirac" $ property $
            \(x :: Rational) y ->
                convolve (dirac x) (dirac y)
                    ===  dirac (x + y)

        it "total" $ property $ mapSize (`div` 10) $
            \mx (my :: Measure Rational) ->
                total (convolve mx my)
                    ===  total mx * total my

        it "dirac translate, left" $ property $ mapSize (`div` 10) $
            \(mx :: Measure Rational) (y :: Rational) ->
                convolve mx (dirac y)
                    ===  translate y mx

        it "dirac translate, right" $ property $ mapSize (`div` 10) $
            \(x :: Rational) (my :: Measure Rational) ->
                convolve (dirac x) my
                    ===  translate x my

        it "symmetric" $ property $ mapSize (`div` 10) $
            \mx (my :: Measure Rational) ->
                convolve mx my
                    ===  convolve my mx

        it "distributive, left" $ property $ mapSize (`div` 12) $
            \mx my (mz :: Measure Rational) ->
                convolve (add mx my) mz
                    ===  add (convolve mx mz) (convolve my mz) 

        it "distributive, right" $ property $ mapSize (`div` 12) $
            \mx my (mz :: Measure Rational) ->
                convolve mx (add my mz)
                    === add (convolve mx my) (convolve mx mz) 

{-----------------------------------------------------------------------------
    Random generators
------------------------------------------------------------------------------}
genMeasure :: Gen (Measure Rational)
genMeasure =
    fromJust . fromDistribution . setLastPieceConstant <$> genPiecewise genPoly
  where
    setLastPieceConstant =
        Piecewise.fromAscPieces
        . setLastPieceConstant'
        . Piecewise.toAscPieces

    setLastPieceConstant' [] = []
    setLastPieceConstant' [(x, o)] = [(x, Poly.constant (eval o x))]
    setLastPieceConstant' (p : ps) = p : setLastPieceConstant' ps

instance Arbitrary (Measure Rational) where
    arbitrary = genMeasure
