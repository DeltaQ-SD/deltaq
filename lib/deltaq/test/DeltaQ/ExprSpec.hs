{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{-|
Copyright   : PLWORKZ R&D, 2025
License     : BSD-3-Clause
-}
module DeltaQ.ExprSpec
    ( spec
    ) where

import Prelude

import DeltaQ.Expr
    ( O
    , Term (..)
    , everything
    , isNormalizedAssoc
    , isVar
    , normalizeAssoc
    , outcomeFromTerm
    , termFromOutcome
    )
import DeltaQ.Gen.Term
    ( genTerm
    )
import Test.Hspec
    ( Spec
    , describe
    , it
    )
import Test.QuickCheck
    ( Gen
    , Arbitrary
    , arbitrary
    , property
    , scale
    , vector
    )

{-----------------------------------------------------------------------------
    Tests
------------------------------------------------------------------------------}
spec :: Spec
spec = do
    describe "Term normalization" $ do
        it "isNormalizedAssoc . normalizeAssoc" $ property $
            \(t :: Term String) ->
                isNormalizedAssoc (normalizeAssoc t)

        it "isNormalizedAssoc . termFromOutcome" $ property $
            \o ->
                isNormalizedAssoc (termFromOutcome o)

    describe "Bulk operations" $ do
        it "(>>=) substitutes" $ property $
            \(t :: Term String) ->
                everything (&&) (not . isVar) (t >>= const Never)

{-----------------------------------------------------------------------------
    Random generators
------------------------------------------------------------------------------}
instance Arbitrary (Term String) where
    arbitrary = scale (`div` 11) (genTerm genName)
      where genName = vector 2 :: Gen String

instance Arbitrary O where
    arbitrary = outcomeFromTerm <$> arbitrary
