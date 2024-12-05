{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{-|
Copyright   : Predictable Network Solutions Ltd., 2024
License     : BSD-3-Clause
Maintainer  : peter.thompson@pnsol.com
-}
module Numeric.Measure.DiscreteSpec
    ( spec
    ) where

import Prelude

import Data.Function.Class
    ( eval
    )
import Numeric.Measure.Discrete
    ( Discrete
    , add
    , convolve
    , dirac
    , distribution
    , fromMap
    , scale
    , toMap
    , total
    , translate
    , zero
    )
import Test.Hspec
    ( Spec
    , describe
    , it
    )
import Test.QuickCheck
    ( Arbitrary
    , (===)
    , (==>)
    , arbitrary
    , cover
    , property
    )

import qualified Data.Map.Strict as Map

{-----------------------------------------------------------------------------
    Tests
------------------------------------------------------------------------------}
spec :: Spec
spec = do
    describe "instance Eq" $ do
        it "add m (scale (-1) m) == zero" $ property $
            \(m :: Discrete Rational) ->
                cover 80 (total m /= 0) "nontrivial"
                $ add m (scale (-1) m)  ===  zero

        it "dirac x /= dirac y" $ property $
            \(x :: Rational) (y :: Rational) ->
                x /= y  ==>  dirac x /= dirac y

    describe "distribution" $ do
        it "eval and total" $ property $
            \(m :: Discrete Rational) ->
                let xlast = maybe 0 fst $ Map.lookupMax $ toMap m
                in  total m
                        === eval (distribution m) xlast

        it "eval and scale" $ property $
            \(m :: Discrete Rational) x s->
                eval (distribution (scale s m)) x
                    === s * eval (distribution m) x

    describe "translate" $ do
        it "distribution" $ property $
            \(m :: Discrete Rational) y x ->
                eval (distribution (translate y m)) x
                    ===  eval (distribution m) (x - y)

    describe "convolve" $ do
        it "dirac" $ property $
            \(x :: Rational) y ->
                convolve (dirac x) (dirac y)
                    ===  dirac (x + y)

        it "distributes over `add`, left" $ property $
            \mx my (mz :: Discrete Rational) ->
                convolve (add mx my) mz
                    === add (convolve mx mz) (convolve my mz) 

        it "distributes over `add`, right" $ property $
            \mx my (mz :: Discrete Rational) ->
                convolve mx (add my mz)
                    === add (convolve mx my) (convolve mx mz) 

        it "total" $ property $
            \mx (my :: Discrete Rational) ->
                total (convolve mx my)
                    === total mx * total my

{-----------------------------------------------------------------------------
    Random generators
------------------------------------------------------------------------------}
instance (Ord a, Num a, Arbitrary a) => Arbitrary (Discrete a) where
    arbitrary = fromMap . Map.fromList <$> arbitrary
