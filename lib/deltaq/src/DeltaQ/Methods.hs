{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-|
Copyright   : Predictable Network Solutions Ltd., 2003-2024
License     : BSD-3-Clause
Description : Methods for analysis and construction.

This module collects common methods
for constructing 'DeltaQ' and analyzing them.
-}
module DeltaQ.Methods
    ( -- * Slack / Hazard
      meetsRequirement
    , SlackOrHazard (..)
    , isSlack
    , isHazard
    
    -- * Timeouts
    , retryOverlapN
    , retryOverlap
    ) where

import DeltaQ.Class
    ( DeltaQ (..)
    , Eventually (..)
    , Outcome (..)
    , eventually
    )
import DeltaQ.PiecewisePolynomial
    ( DQ
    , timeout
    )

{-----------------------------------------------------------------------------
    Methods
    Slack / Hazard
------------------------------------------------------------------------------}
-- | The \"slack or hazard\" represents the distance between
-- a reference point in (time, probability) space
-- and a given 'DeltaQ'.
--
-- * 'Slack' represents the case where the 'DeltaQ' __meets__
--   the performance requirements set by the reference point.
-- * 'Hazard' represents the case where the 'DeltaQ' __fails__ to meet
--   the performance requirements set by the reference point.
--
-- Both cases include information of how far the reference point is
-- away.
data SlackOrHazard o
    = Slack (Duration o) (Probability o)
    -- ^ We have some slack.
    -- Specifically, we have 'Duration' at the same probability as the reference,
    -- and 'Probability' at the same duration as the reference.
    | Hazard (Eventually (Duration o)) (Probability o)
    -- ^ We fail to meet the reference point.
    -- Specifically,
    -- we overshoot by 'Duration' at the same probability as the reference,
    -- and by 'Probability' at the same duration as the reference.

deriving instance (Eq (Duration o), Eq (Probability o))
    => Eq (SlackOrHazard o)

deriving instance (Show (Duration o), Show (Probability o))
    => Show (SlackOrHazard o)

-- | Test whether the given 'SlackOrHazard' is 'Slack'.
isSlack :: SlackOrHazard o -> Bool
isSlack (Slack _ _) = True
isSlack _ = False

-- | Test whether the given 'SlackOrHazard' is 'Hazard'.
isHazard :: SlackOrHazard o -> Bool
isHazard (Hazard _ _) = True
isHazard _ = False

-- | Compute \"slack or hazard\" with respect to a given reference point.
meetsRequirement
    :: DeltaQ o => o -> (Duration o, Probability o) -> SlackOrHazard o
meetsRequirement o (t,p)
    | dp >= 0 = Slack dt dp
    | Abandoned <- t' = Hazard Abandoned (negate dp)
    | otherwise = Hazard (Occurs $ negate dt) (negate dp)
  where
    dp = p' - p
    dt = t - eventually err id t'

    t' = quantile o p
    p' = o `successWithin` t

    err = error "distanceToReference: inconsistency"

{-----------------------------------------------------------------------------
    Methods
    Timeouts
------------------------------------------------------------------------------}

-- | Retry an outcome @n@ times, take the first instance that succeeds.
--
-- More specifically, @retryOverlapN n dt o@ models the completion times
-- of a race of @n@ independent copies of the outcome @o@ against each other,
-- succeeding when any of the copies first succeeds.
--
-- Copy number @0 <= j < n@ is started after waiting a time @j * dt@.
--
-- If no copy succeeds within total time @n * dt@,
-- the whole outcome fails.
retryOverlapN :: Int -> Duration DQ -> DQ -> DQ
retryOverlapN n dt = retryOverlap (replicate n dt)

-- | Retry an outcome multiple times, take the first instance that succeeds.
--
-- More specifically, @retryOverlap dts o@ models the completion times
-- of a race of @length dts@ independent copies of the outcome @o@
-- against each other, succeeding when any of the copies first succeeds.
--
-- Each copy is started after a timeout
-- in the event that the previous copies have not finished yet;
-- the list @dts@ gives the successive timeouts.
--
-- If no copy succeeds within total time @sum dts@,
-- the whole outcome fails.
retryOverlap :: [Duration DQ] -> DQ -> DQ
retryOverlap dts0 o = go dts0 o
  where
    go [] _ = never
    go (dt:dts) race =
        choice p before (wait dt .>>. retry)
      where
        (before, p, after) = timeout dt race

        -- add another copy of `o` to the race
        retry = go dts (after .\/. o)
