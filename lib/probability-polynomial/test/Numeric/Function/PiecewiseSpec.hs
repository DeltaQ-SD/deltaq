{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}

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

import Data.Function.Class
    ( eval
    )
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
    ( Arbitrary
    , Positive (..)
    , (===)
    , arbitrary
    , property
    )

import qualified Data.Function.Class as Fun

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

        it "eval" $ property $
            \(x :: Rational) (Positive d) (o :: Linear) z ->
                let y = x + d
                    p = fromInterval (x, y) o
                in 
                    eval p z
                        === (if x <= z && z < y then eval o z else 0)

{-----------------------------------------------------------------------------
    Helper types
------------------------------------------------------------------------------}
type Q = Rational

-- | Linear function with a constant and a slope
data Linear = Linear Q Q
    deriving (Eq, Show)

instance Num Linear where
    Linear a1 b1 + Linear a2 b2 = Linear (a1 + a2) (b1 + b2)
    fromInteger n = Linear 0 (fromInteger n)

instance Fun.Function Linear where
    type instance Domain Linear = Q
    type instance Codomain Linear = Q
    eval = evalLinear

evalLinear :: Linear -> Q -> Q
evalLinear (Linear a b) x = a*x + b

{-----------------------------------------------------------------------------
    Random generators
------------------------------------------------------------------------------}
instance Arbitrary Linear where
    arbitrary = Linear <$> arbitrary <*> arbitrary
