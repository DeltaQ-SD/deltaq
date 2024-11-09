{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module DeltaQ.PiecewisePolynomialSpec
    ( spec
    ) where

import Prelude

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
