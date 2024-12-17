{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{-|
Copyright   : Predictable Network Solutions Ltd., 2024
License     : BSD-3-Clause
Maintainer  : neil.davies@pnsol.com
-}
module DeltaQ.MethodsSpec
    ( spec
    ) where

import Prelude

import DeltaQ.Class
    ( Eventually (..)
    , DeltaQ (..)
    , Outcome (..)
    )
import DeltaQ.PiecewisePolynomial
    ( DQ
    )
import DeltaQ.Methods
    ( SlackOrHazard (..)
    , meetsRequirement
    )
import Test.Hspec
    ( Spec
    , describe
    , it
    )
import Test.QuickCheck
    ( NonNegative (..)
    , Positive (..)
    , property
    , withMaxSuccess
    )

{-----------------------------------------------------------------------------
    Tests
------------------------------------------------------------------------------}
spec :: Spec
spec = do
    describe "SlackOrHazard" $ do
        it "never, hazard" $ withMaxSuccess 1 $ property $
            let deltaq = never :: DQ
            in  case deltaq `meetsRequirement` (1, 0.9) of
                    Hazard Abandoned dp -> dp >= 0
                    _ -> False

        it "uniform, slack" $ property $
            \(NonNegative r) (Positive d) ->
                let s = r + d
                    deltaq = uniform r s :: DQ
                in  case deltaq `meetsRequirement` (s + d, 0.9) of
                        Slack dt dp -> dp >= 0 && dt >= 0
                        _ -> False

        it "uniform, hazard" $ property $
            \(NonNegative r) (Positive d) ->
                let s = r + d
                    deltaq = uniform r s :: DQ
                in  case deltaq `meetsRequirement` (0, 0.9) of
                        Hazard (Occurs dt) dp -> dt >= 0 && dp >= 0
                        _ -> False
