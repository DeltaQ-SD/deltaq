{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{-|
Copyright   : Predictable Network Solutions Ltd., 2024
License     : BSD-3-Clause
Maintainer  : peter.thompson@pnsol.com
Description : Tests for discrete finite signed measures.
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
    )
import Test.Hspec
    ( Spec
    , describe
    , it
    )
import Test.QuickCheck
    ( Arbitrary
    , (===)
    , arbitrary
    , property
    )

import qualified Data.Map.Strict as Map

{-----------------------------------------------------------------------------
    Tests
------------------------------------------------------------------------------}
spec :: Spec
spec = do
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

    describe "convolve" $ do
        it "dirac" $ property $
            \(x :: Rational) wx y wy ->
                convolve (dirac x wx) (dirac y wy)
                    ===  dirac (x + y) (wx * wy)

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
