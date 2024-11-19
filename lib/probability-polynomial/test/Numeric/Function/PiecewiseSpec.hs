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
    ( Piecewise
    , fromAscPieces
    , fromInterval
    , intervals
    )
import Test.Hspec
    ( Spec
    , describe
    , it
    )
import Test.QuickCheck
    ( Arbitrary
    , Gen
    , Positive (..)
    , (===)
    , arbitrary
    , frequency
    , listOf
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

    describe "Interval" $ do
        it "member intersect" $ property $
            \x y z ->
                member z (intersect x y)  ===  (member z x && member z y)


{-----------------------------------------------------------------------------
    Helper types
    Linear functions
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
    Helper types
    Intervals
------------------------------------------------------------------------------}
-- | Interval on the real number line.
-- This type does not represent all interval types,
-- only those that are relevant to our purposes here.
data Interval
    = All
    | Empty
    | Before Q  -- exclusive
    | After Q   -- inclusive
    | FromTo Q Q
    deriving (Eq, Show)

-- | Definition of membership.
member :: Q -> Interval -> Bool
member _ All = True
member _ Empty = False
member z (Before y) = z < y
member z (After x) = x <= z
member z (FromTo x y) = x <= z && z < y

-- | The intersection of two 'Interval' is again an 'Interval'.
intersect :: Interval -> Interval -> Interval
intersect All x = x
intersect x All = x
intersect Empty _ = Empty
intersect _ Empty = Empty
intersect (Before y1) (Before y2) = Before (min y1 y2)
intersect (Before y1) (After x2) = mkFromTo x2 y1
intersect (Before y1) (FromTo x2 y2) = mkFromTo x2 (min y1 y2)
intersect (After x1) (After x2) = After (max x1 x2)
intersect (After x1) (Before y2) = mkFromTo x1 y2
intersect (After x1) (FromTo x2 y2) = mkFromTo (max x1 x2) y2
intersect (FromTo x1 y1) (Before y2) = mkFromTo x1 (min y1 y2)
intersect (FromTo x1 y1) (After x2) = mkFromTo (max x1 x2) y1
intersect (FromTo x1 y1) (FromTo x2 y2) = mkFromTo (max x1 x2) (min y1 y2)

-- | Smart constructor,
-- returns 'Empty' if the endpoint does not come after the starting point.
mkFromTo :: Q -> Q -> Interval
mkFromTo x y = if x < y then FromTo x y else Empty

-- | Return all intervals, 
allIntervals :: Piecewise Q o -> [Interval]
allIntervals pieces
    | null is = [All]
    | otherwise = [Before xmin] <> map (uncurry FromTo) is <> [After xmax]
  where
    is = intervals pieces
    xmin = minimum (map fst is)
    xmax = maximum (map snd is)

{-----------------------------------------------------------------------------
    Random generators
------------------------------------------------------------------------------}
instance Arbitrary Linear where
    arbitrary = Linear <$> arbitrary <*> arbitrary

genInterval :: Gen (Q,Q)
genInterval = do
    x <- arbitrary
    Positive d <- arbitrary
    pure (x, x + d)

genFromTo :: Gen Interval
genFromTo = uncurry FromTo <$> genInterval

instance Arbitrary Interval where
    arbitrary = frequency
        [ (1, pure All)
        , (1, pure Empty)
        , (3, Before <$> arbitrary)
        , (3, After <$> arbitrary)
        , (20, genFromTo)
        ]

-- | A list of disjoint and sorted elements.
newtype DisjointSorted a = DisjointSorted [a]
    deriving (Eq, Show)

instance Arbitrary (DisjointSorted Rational) where
    arbitrary =
        DisjointSorted . drop 1 . scanl (\s (Positive d) -> s + d) 0
            <$> listOf arbitrary

instance Arbitrary o => Arbitrary (Piecewise Rational o) where
    arbitrary = do
        DisjointSorted xs <- arbitrary
        os <- mapM (const arbitrary) xs
        pure $ fromAscPieces $ zip xs os
