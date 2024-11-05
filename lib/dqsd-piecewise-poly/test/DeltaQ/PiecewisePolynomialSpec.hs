{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module DeltaQ.PiecewisePolynomialSpec
    ( spec
    ) where

import Prelude

import DeltaQ.Class
    ( Outcome (..)
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
    , pending
    )
import Test.QuickCheck
    ( NonNegative (..)
    , Property
    , (===)
    , property
    )

{-----------------------------------------------------------------------------
    Tests
------------------------------------------------------------------------------}
-- | '(===)' with constrained types.
(.===) :: Durations Rational -> Durations Rational -> Property
(.===) = (===)

spec :: Spec
spec = do
    describe "pending" $
        it "pending" $
            pending

{-----------------------------------------------------------------------------
    Random generators
------------------------------------------------------------------------------}
