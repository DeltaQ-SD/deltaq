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
    , Eventually (..)
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
    , (==>)
    , arbitrary
    , choose
    , chooseInteger
    , frequency
    , getSize
    , mapSize
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

    describe ".>>." $ do
        it "associativity" $ property $ mapSize (`div` 3) $
            \x y z ->
                (x .>>. y) .>>. z .=== x .>>. (y .>>. z)

    describe "./\\." $ do
        xit "associativity" $ property $
            \x y z ->
                (x ./\. y) ./\. z .=== x ./\. (y ./\. z)

    describe ".\\/." $ do
        xit "associativity" $ property $
            \x y z ->
                (x .\/. y) .\/. z .=== x .\/. (y .\/. z)

    describe "choice" $ do
        it ".>>." $ property $ mapSize (`div` 3) $
            \(Probability p) x y z ->
                choice p x y .>>. z  .===  choice p (x .>>. z) (y .>>. z)
        xit "./\\." $ property $
            \(Probability p) x y z ->
                choice p x y ./\. z  .===  choice p (x ./\. z) (y ./\. z)
        xit ".\\/." $ property $
            \(Probability p) x y z ->
                choice p x y .\/. z  .===  choice p (x .\/. z) (y .\/. z)

    describe "uniform" $ do
        xit "wait .>>. uniform" $ property $
            \(NonNegative r) (NonNegative s) (NonNegative t) ->
                (wait t .>>. uniform r s) .=== uniform (t+r) (t+s)
        xit "uniform .>>. wait" $ property $
            \(NonNegative r) (NonNegative s) (NonNegative t) ->
                (uniform r s .>>. wait t) .=== uniform (r+t) (s+t)

    describe "failure" $ do
        let failure' :: Durations Rational -> Rational
            failure' = failure

        it "never" $ property $
            failure' never  ===  1
        xit "wait" $ property $
            \(NonNegative t) ->
                failure' (wait t)  ===  1
        xit ".>>." $ property $
            \x y ->
                failure' (x .>>. y)
                    ===  1 - (1 - failure' x) * (1 - failure' y)
        xit "./\\." $ property $
            \x y ->
                failure' (x ./\. y)
                    ===  1 - (1 - failure' x) * (1 - failure' y)
        xit ".\\/." $ property $
            \x y ->
                failure' (x .\/. y)
                    ===  failure' x * failure' y
        xit "choice" $ property $
            \(Probability p) x y ->
                failure' (choice p x y)
                    ===  p * failure' x + (1-p) * failure' y
        xit "uniform" $ property $
            \(NonNegative r) (NonNegative s) ->
                failure' (uniform r s)  ===  0

    describe "successBefore" $ do
        let successBefore' :: Durations Rational -> Rational -> Rational
            successBefore' = successBefore

        xit "never" $ property $
            \(NonNegative t) ->
                successBefore' never t  ===  0
        xit "wait" $ property $
            \(NonNegative t) (NonNegative s) ->
                successBefore' (wait s) t  ===  if t <= s then 0 else 1
        xit "./\\." $ property $
            \(NonNegative t) x y ->
                successBefore' (x ./\. y) t
                    === successBefore' x t * successBefore' y t
        xit ".\\/." $ property $
            \(NonNegative t) x y ->
                successBefore' (x .\/. y) t
                    === 1 - (1 - successBefore' x t) * (1 - successBefore' y t)
        xit "choice" $ property $
            \(NonNegative t) (Probability p) x y ->
                successBefore' (choice p x y) t
                    ===  p * successBefore' x t + (1-p) * successBefore' y t
        it "uniform" $ property $ 
            let successBefore2 r s t
                    | t <= r          = 0
                    | r < t && t <= s = (t-r) / (s-r)
                    | s < t           = 1
                    | otherwise       = error "impossible"
            in \(NonNegative t) (NonNegative r) (Positive d) ->
                let s = r + d
                in  successBefore' (uniform r s) t
                        === successBefore2 r s t

    describe "quantile" $ do
        let quantile' :: Rational -> Durations Rational -> Eventually Rational
            quantile' = quantile

        xit "monotonic" $ property $
            \o (Probability p) (Probability q) ->
                let p' = min p q
                    q' = max p q
                in
                    p' <= q'  ==>  quantile' p' o <= quantile' q' o

        it "uniform" $ property $
            \(Probability p) (NonNegative r) (Positive d) ->
                let s = r + d
                in  quantile' p (uniform r s) === Occurs (r + p*(s-r))

    describe "earliest" $ do
        let earliest' :: Durations Rational -> Eventually Rational
            earliest' = earliest

        xit "never" $ property $
            earliest' never  ===  Abandoned
        it "wait" $ property $
            \(NonNegative t) ->
                earliest' (wait t)  ===  Occurs t
        xit ".>>." $ property $
            \x y ->
                earliest' (x .>>. y)
                    ===  ((+) <$> earliest' x <*> earliest' y)
        xit "./\\." $ property $
            \x y ->
                earliest' (x ./\. y)
                    ===  max (earliest' x) (earliest' y)
        xit ".\\/." $ property $
            \x y ->
                earliest' (x ./\. y)
                    ===  min (earliest' x) (earliest' y)
        xit "choice" $ property $
            \(Probability p) x y ->
                (0 < p && p < 1) ==>
                    (earliest' (choice p x y)
                        === min (earliest' x) (earliest' y))
        xit "uniform" $ property $
            \(NonNegative r) (NonNegative s) ->
                earliest' (uniform r s)  ===  Occurs (min r s)

    describe "deadline" $ do
        let deadline' :: Durations Rational -> Eventually Rational
            deadline' = deadline

        xit "never" $ property $
            deadline' never  ===  Abandoned
        it "wait" $ property $
            \(NonNegative t) ->
                deadline' (wait t)  ===  Occurs t
        xit ".>>." $ property $
            \x y ->
                deadline' (x .>>. y)
                    ===  ((+) <$> deadline' x <*> deadline' y)
        xit "./\\." $ property $
            \x y ->
                deadline' (x ./\. y)
                    ===  max (deadline' x) (deadline' y)
        xit ".\\/." $ property $
            \x y ->
                deadline' (x ./\. y)
                    ===  min (deadline' x) (deadline' y)
        xit "choice" $ property $
            \(Probability p) x y ->
                (0 < p && p < 1) ==>
                    (deadline' (choice p x y)
                        === max (deadline' x) (deadline' y))
        xit "uniform" $ property $
            \(NonNegative r) (NonNegative s) ->
                deadline' (uniform r s)  ===  Occurs (max r s)

{-----------------------------------------------------------------------------
    Random generators
------------------------------------------------------------------------------}
data Prob = Probability Rational
    deriving (Eq, Show)

instance Arbitrary Prob where
    arbitrary = Probability <$> genProbability

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
