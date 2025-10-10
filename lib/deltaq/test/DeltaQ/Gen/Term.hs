{-|
Copyright   : PLWORKZ R&D, 2025
License     : BSD-3-Clause
-}
module DeltaQ.Gen.Term
    ( genProbability
    , genTerm
    ) where

import Prelude

import Data.Ratio
    ( (%)
    )
import DeltaQ.Expr
    ( Term (..)
    )
import Test.QuickCheck
    ( Gen
    , NonNegative (..)
    , arbitrary
    , choose
    , chooseInteger
    , frequency
    , getSize
    , oneof
    , vectorOf
    )

{-----------------------------------------------------------------------------
    Random generators
------------------------------------------------------------------------------}
-- | Generate a random outcome expression.
genTerm :: Gen v -> Gen (Term v)
genTerm genName = do
    size <- getSize
    genDeltaQFromList =<< vectorOf size (genSimpleOutcome genName)

-- | Generate a deterministic outcome 'wait'.
genWait :: Gen (Term v)
genWait = do
    NonNegative a <- arbitrary
    pure $ Wait a

-- | Generate a 'var' with a short name.
genVar :: Gen v -> Gen (Term v)
genVar = fmap Var

-- | Generate a simple outcome.
genSimpleOutcome :: Gen v -> Gen (Term v)
genSimpleOutcome genName =
    frequency
        [ (20, genVar genName)
        , ( 4, genWait)
        , ( 2, pure Wait0)
        , ( 4, pure Never)
        , ( 1, pure $ Loc "test")
        ]

-- | Generate a random probability between (0,1) an
genProbability :: Gen Rational
genProbability = do
    denominator <- chooseInteger (1,2^(20 :: Int))
    numerator <- chooseInteger (0, denominator)
    pure (numerator % denominator)

-- | Generate a random 'DeltaQ' by combining a given list
-- of outcomes with random operations.
genDeltaQFromList :: [Term v] -> Gen (Term v)
genDeltaQFromList [] = pure Never
genDeltaQFromList [x] = pure x
genDeltaQFromList xs = do
    n <- choose (1, length xs - 1)
    let (ys, zs) = splitAt n xs
    genOp <*> genDeltaQFromList ys <*> genDeltaQFromList zs
  where
    ops = [\a b -> Seq [a,b], \a b -> First [a,b], \a b -> Last [a,b]]
    genChoice = do
        p <- genProbability
        pure $ \a b -> Choices [(1-p, a), (p, b)]
    genOp = oneof $ map pure ops <> [genChoice]
