{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{-|
Copyright   : Predictable Network Solutions Ltd., 2024
License     : BSD-3-Clause
Maintainer  : neil.davies@pnsol.com
-}
module DeltaQ.PiecewisePolynomialSpec
    ( spec
    ) where

import Prelude

import Data.Maybe
    ( fromJust
    )
import Data.Ratio
    ( (%)
    )
import DeltaQ.Class
    ( DeltaQ (..)
    , Eventually (..)
    , Outcome (..)
    )
import DeltaQ.PiecewisePolynomial
    ( DQ
    , complexity
    , distribution
    , fromPositiveMeasure
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
    , (.&&.)
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
    , withMaxSuccess
    )

import qualified Numeric.Measure.Finite.Mixed as Measure

{-----------------------------------------------------------------------------
    Tests
------------------------------------------------------------------------------}
infix 0 .===

-- | '(===)' with constrained types.
(.===) :: DQ -> DQ -> Property
(.===) = (===)

spec :: Spec
spec = do
    describe "general DeltaQ properties" specProperties
    describe "DQ specifics" specImplementation

specProperties :: Spec
specProperties = do
    describe "never" $ do
        it "x .>>. never" $ property $
            \x ->
                (x .>>. never) .=== never

        it "x ./\\. never" $ property $
            \x ->
                (x ./\. never) .=== never

        it "x .\\/. never" $ property $
            \x ->
                (x .\/. never) .=== x

        it "never .>>. x" $ property $
            \x ->
                (never .>>. x) .=== never

        it "never ./\\. x" $ property $
            \x ->
                (never ./\. x) .=== never

        it "never .\\/. x" $ property $
            \x ->
                (never .\/. x) .=== x

    describe "wait" $ do
        it ".>>." $ property $
            \(NonNegative t) (NonNegative s) ->
                (wait t .>>. wait s) .=== wait (t+s)

        it "./\\." $ property $
            \(NonNegative t) (NonNegative s) ->
                (wait t ./\. wait s) .=== wait (max t s)

        it ".\\/." $ property $
            \(NonNegative t) (NonNegative s) ->
                (wait t .\/. wait s) .=== wait (min t s)

    describe ".>>." $ do
        it "associativity" $ property $ mapSize (`div` 3) $
            \x y z ->
                (x .>>. y) .>>. z .=== x .>>. (y .>>. z)

    describe "./\\." $ do
        it "associativity" $ property $
            \x y z ->
                (x ./\. y) ./\. z .=== x ./\. (y ./\. z)

        it "commutativity" $ property $
            \x y ->
                x ./\. y  .===  y ./\. x

    describe ".\\/." $ do
        it "associativity" $ property $
            \x y z ->
                (x .\/. y) .\/. z .=== x .\/. (y .\/. z)

        it "commutativity" $ property $
            \x y ->
                x .\/. y  .===  y .\/. x

    describe "choice" $ do
        it "choice 1" $ property $
            \x y ->
                choice 1 x y  .===  x

        it "choice 0" $ property $
            \x y ->
                choice 0 x y  .===  y

        it ".>>." $ property $ mapSize (`div` 3) $
            \(Probability p) x y z ->
                choice p x y .>>. z  .===  choice p (x .>>. z) (y .>>. z)

        it "./\\." $ property $
            \(Probability p) x y z ->
                choice p x y ./\. z  .===  choice p (x ./\. z) (y ./\. z)

        it ".\\/." $ property $
            \(Probability p) x y z ->
                choice p x y .\/. z  .===  choice p (x .\/. z) (y .\/. z)

    describe "choices" $ do
        it "choices []" $ property $
            (choices []  .===   never)

        it "choices ((w,o):wos)" $ property $ mapSize (`div` 2) $
            \ (Positive (w :: Rational))
              (o :: DQ)
              (wos' :: [(Positive Rational, DQ)]) ->
                let wos = map (\(Positive w', o') -> (w',o')) wos'
                    ws = map fst wos
                    p = w / (w + sum ws)
                in
                    choices ((w,o):wos)
                        .=== choice p o (choices wos)

    describe "uniform" $ do
        it "wait .>>. uniform" $ property $
            \(NonNegative r) (Positive d) (NonNegative t) ->
                let s = r + d in
                (wait t .>>. uniform r s) .=== uniform (t+r) (t+s)

        it "uniform .>>. wait" $ property $
            \(NonNegative r) (Positive d) (NonNegative t) ->
                let s = r + d in
                (uniform r s .>>. wait t) .=== uniform (r+t) (s+t)

    describe "failure" $ do
        let failure' :: DQ -> Rational
            failure' = failure

        it "never" $ property $
            failure' never  ===  1

        it "wait" $ property $
            \(NonNegative t) ->
                failure' (wait t)  ===  0

        it ".>>." $ property $
            \x y ->
                failure' (x .>>. y)
                    ===  1 - (1 - failure' x) * (1 - failure' y)

        it "./\\." $ property $
            \x y ->
                failure' (x ./\. y)
                    ===  1 - (1 - failure' x) * (1 - failure' y)

        it ".\\/." $ property $
            \x y ->
                failure' (x .\/. y)
                    ===  failure' x * failure' y

        it "choice" $ property $
            \(Probability p) x y ->
                failure' (choice p x y)
                    ===  p * failure' x + (1-p) * failure' y

        it "uniform" $ property $
            \(NonNegative r) (Positive d) ->
                let s = r + d in
                failure' (uniform r s)  ===  0

    describe "successWithin" $ do
        let successWithin' :: DQ -> Rational -> Rational
            successWithin' = successWithin

        it "never" $ property $
            \(NonNegative t) ->
                successWithin' never t  ===  0

        it "wait" $ property $
            \(NonNegative t) (NonNegative s) ->
                successWithin' (wait s) t  ===  if t < s then 0 else 1

        it "./\\." $ property $
            \(NonNegative t) x y ->
                successWithin' (x ./\. y) t
                    === successWithin' x t * successWithin' y t

        it ".\\/." $ property $
            \(NonNegative t) x y ->
                successWithin' (x .\/. y) t
                    === 1 - (1 - successWithin' x t) * (1 - successWithin' y t)

        it "choice" $ property $
            \(NonNegative t) (Probability p) x y ->
                successWithin' (choice p x y) t
                    ===  p * successWithin' x t + (1-p) * successWithin' y t

        it "uniform" $ property $ 
            let successWithin2 r s t
                    | t < r           = 0
                    | r <= t && t < s = (t-r) / (s-r)
                    | s <= t          = 1
                    | otherwise       = error "impossible"
            in \(NonNegative t) (NonNegative r) (Positive d) ->
                let s = r + d
                in  successWithin' (uniform r s) t
                        === successWithin2 r s t

    describe "quantile" $ do
        let quantile' :: Rational -> DQ -> Eventually Rational
            quantile' = quantile

        it "monotonic" $ property $
            \o (Probability p) (Probability q) ->
                let p' = min p q
                    q' = max p q
                in
                    p' <= q'  ==>  quantile' p' o <= quantile' q' o

        it "never" $ property $
            \(Probability p) ->
                quantile' p never  ===  Abandoned

        it "wait" $ property $
            \(Probability p) (NonNegative t) ->
                quantile' p (wait t)
                    ===  if p >= 0 then Occurs t else Abandoned

        it "uniform" $ property $
            \(Probability p) (NonNegative r) (Positive d) ->
                let s = r + d in 
                quantile' p (uniform r s)
                    === Occurs (r + p*(s-r))

    describe "earliest" $ do
        let earliest' :: DQ -> Eventually Rational
            earliest' = earliest

        it "never" $ property $
            earliest' never  ===  Abandoned

        it "wait" $ property $
            \(NonNegative t) ->
                earliest' (wait t)  ===  Occurs t

        it ".>>." $ property $
            \x y ->
                earliest' (x .>>. y)
                    ===  ((+) <$> earliest' x <*> earliest' y)

        it "./\\." $ property $
            \x y ->
                earliest' (x ./\. y)
                    ===  max (earliest' x) (earliest' y)

        it ".\\/." $ property $
            \x y ->
                earliest' (x .\/. y)
                    ===  min (earliest' x) (earliest' y)

        it "choice" $ property $
            \(Probability p) x y ->
                (0 < p && p < 1) ==>
                    (earliest' (choice p x y)
                        === min (earliest' x) (earliest' y))

        it "uniform" $ property $
            \(NonNegative r) (NonNegative s) ->
                earliest' (uniform r s)  ===  Occurs (min r s)

    describe "deadline" $ do
        let deadline' :: DQ -> Eventually Rational
            deadline' = deadline

        it "never" $ property $
            deadline' never  ===  Abandoned

        it "wait" $ property $
            \(NonNegative t) ->
                deadline' (wait t)  ===  Occurs t

        it ".>>." $ property $
            \x y ->
                deadline' (x .>>. y)
                    ===  ((+) <$> deadline' x <*> deadline' y)

        it "./\\." $ property $
            \x y ->
                deadline' (x ./\. y)
                    ===  max (deadline' x) (deadline' y)

        xit ".\\/." $ property $
            \x y ->
                deadline' (x .\/. y)
                    ===  min (deadline' x) (deadline' y)

        xit "choice" $ property $
            \(Probability p) x y ->
                (0 < p && p < 1) ==>
                    deadline' (choice p x y)
                        === max (deadline' x) (deadline' y)

        it "uniform" $ property $
            \(NonNegative r) (NonNegative s) ->
                deadline' (uniform r s)  ===  Occurs (max r s)

specImplementation :: Spec
specImplementation = do
    describe "fromPositiveMeasure" $ do
        it "fails on negative measure" $ property $
            \(NonNegative r) (Positive d) ->
                let s = r + d in
                fromPositiveMeasure
                    (Measure.scale (-1) (Measure.uniform r s))
                    === Nothing

    describe "fromPositiveMeasure . distribution" $ do
        it "uniform" $ property $
            \(NonNegative r) (Positive d) ->
                let s = r + d
                    id' =
                        fromPositiveMeasure
                        . fromJust
                        . Measure.fromDistribution
                        . distribution
                in
                    id' (uniform r s) === Just (uniform r s)

    describe "complexity" $ do
        it "grows exponentially with .>>." $ withMaxSuccess 1 $ property $
            let power2 (n :: Int) = choice (1/2) (wait 0) (wait (2^n))
                convolved (m :: Int) = foldr1 (.>>.) $ map power2 [1..m]
            in
                complexity (power2 1) <= 4
                    .&&. complexity (convolved 10) >= 2^(10 :: Int)

{-----------------------------------------------------------------------------
    Random generators
------------------------------------------------------------------------------}
data Prob = Probability Rational
    deriving (Eq, Show)

instance Arbitrary Prob where
    arbitrary = Probability <$> genProbability

instance Arbitrary DQ where
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
