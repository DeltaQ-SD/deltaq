{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module DeltaQ.PiecewisePolynomialSpec
    ( spec
    ) where

import Prelude

import Data.Ratio
    ( (%)
    )
import DeltaQ.Class
    ( DeltaQ (..)
    , Outcome (..)
    , (.>>.)
    , (./\.)
    , (.\/.)
    )
import DeltaQ.PiecewisePolynomial
    ( Durations
    )
import Test.Hspec
    ( Spec
    , describe
    , it
    , xit
    )
import Test.QuickCheck
    ( Arbitrary
    , Gen
    , NonNegative (..)
    , Positive (..)
    , Property
    , (===)
    , arbitrary
    , choose
    , chooseInteger
    , frequency
    , getSize
    , oneof
    , property
    , scale
    , vectorOf
    )

{-----------------------------------------------------------------------------
    Tests
------------------------------------------------------------------------------}
infix 0 .===

-- | '(===)' with constrained types.
(.===) :: Durations Rational -> Durations Rational -> Property
(.===) = (===)

spec :: Spec
spec = do
    describe "never" $ do
        it "x .>>. never" $ property $
            \x ->
                (x .>>. never) .=== never
        xit "x ./\\. never" $ property $
            \x ->
                (x ./\. never) .=== never
        xit "x .\\/. never" $ property $
            \x ->
                (x ./\. never) .=== x

        it "never .>>. x" $ property $
            \x ->
                (never .>>. x) .=== never
        xit "never ./\\. x" $ property $
            \x ->
                (never ./\. x) .=== never
        xit "never .\\/. x" $ property $
            \x ->
                (never ./\. x) .=== x

    describe "wait" $ do
        it ".>>." $ property $
            \(NonNegative t) (NonNegative s) ->
                (wait t .>>. wait s) .=== wait (t+s)
        xit "./\\." $ property $
            \(NonNegative t) (NonNegative s) ->
                (wait t ./\. wait s) .=== wait (max t s)
        xit ".\\/." $ property $
            \(NonNegative t) (NonNegative s) ->
                (wait t .\/. wait s) .=== wait (min t s)

    describe "uniform" $ do
        xit "wait .>>. uniform" $ property $
            \(NonNegative r) (NonNegative s) (NonNegative t) ->
                (wait t .>>. uniform r s) .=== uniform (t+r) (t+s)
        xit "uniform .>>. wait" $ property $
            \(NonNegative r) (NonNegative s) (NonNegative t) ->
                (uniform r s .>>. wait t) .=== uniform (r+t) (s+t)

{-----------------------------------------------------------------------------
    Random generators
------------------------------------------------------------------------------}
instance Arbitrary (Durations Rational) where
    arbitrary = scale (`div` 10) genDeltaQ

-- | Generate a random 'DeltaQ' by generating a random expression.
genDeltaQ
    :: (DeltaQ o, Arbitrary (Duration o), Probability o ~ Rational)
    => Gen o
genDeltaQ = do
    size <- getSize
    genDeltaQFromList =<< vectorOf size genSimpleOutcome

-- | Generate a simple probability distribution using 'uniform'.
genUniform :: (DeltaQ o, Arbitrary (Duration o)) => Gen o
genUniform = do
    NonNegative a <- arbitrary
    Positive d <- arbitrary
    pure $ uniform a (a + d)

-- | Generate a deterministic outcome 'wait'.
genWait :: (Outcome o, Arbitrary (Duration o)) => Gen o
genWait = do
    NonNegative a <- arbitrary
    pure $ wait a

-- | Generate a simple outcome — one of 'uniform', 'wait', or 'never'.
genSimpleOutcome :: (DeltaQ o, Arbitrary (Duration o)) => Gen o
genSimpleOutcome =
    frequency [(20, genUniform), (4, genWait), (1, pure never)]

-- | Generate a random probability between (0,1) an
genProbability :: Gen Rational
genProbability = do
    denominator <- chooseInteger (1,2^(20 :: Int))
    numerator <- chooseInteger (0, denominator)
    pure (numerator % denominator)

-- | Generate a random 'DeltaQ' by combining a given list
-- of outcomes with random operations.
genDeltaQFromList :: (DeltaQ o, Probability o ~ Rational) => [o] -> Gen o
genDeltaQFromList [] = pure never
genDeltaQFromList [x] = pure x
genDeltaQFromList xs = do
    n <- choose (1, length xs - 1)
    let (ys, zs) = splitAt n xs
    genOp <*> genDeltaQFromList ys <*> genDeltaQFromList zs
  where
    genChoice = do
        p <- genProbability
        pure $ choice p 
    genOp = oneof [pure (.>>.), pure (./\.), pure (.\/.), genChoice]
