{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{-|
Copyright   : Predictable Network Solutions Ltd., 2003-2024
License     : BSD-3-Clause
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
    , timeout
    )
import DeltaQ.Methods
    ( SlackOrHazard (..)
    , meetsRequirement
    , retryOverlap
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
    , (===)
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

    describe "retryOverlap" $ do
        it "documentation" $ property $
            \(Positive d) (dts' :: [Positive Rational]) ->
                let r = 0
                    s = r + d
                    dts = map (\(Positive dt) -> dt) dts'
                    o = uniform r s :: DQ
                in
                    retryOverlap dts o  ===  retryOverlap' dts o

-- | Implementation of 'retryOverlap' that matches the description
-- in the documentation. 
retryOverlap' :: [Duration DQ] -> DQ -> DQ
retryOverlap' dts o =
    cutoff (sum dts)
        $ foldr (.\/.) never
            [ wait dt .>>. o
            | dt <- init (scanl (+) 0 dts)
            ]
  where
    cutoff dt x = choice p before never
      where
        (before, p, _) = timeout dt x
