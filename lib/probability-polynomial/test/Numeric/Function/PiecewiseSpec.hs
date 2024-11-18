{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{-|
Copyright   : Predictable Network Solutions Ltd., 2024
License     : BSD-3-Clause
Maintainer  : peter.thompson@pnsol.com
Description : Tests for piecewise functions.
-}
module Numeric.Function.PiecewiseSpec
    ( spec
    ) where

import Prelude

import Numeric.Function.Piecewise
    ( fromInterval
    , intervals
    )
import Test.Hspec
    ( Spec
    , describe
    , it
    )
import Test.QuickCheck
    ( Positive (..)
    , (===)
    , property
    )

{-----------------------------------------------------------------------------
    Tests
------------------------------------------------------------------------------}
spec :: Spec
spec = do
    describe "fromInterval" $ do
        it "intervals" $ property $
            \(x :: Rational) (Positive d) (o :: Rational) ->
                let y = x + d
                in  intervals (fromInterval (x,y) o) === [(x,y)]
