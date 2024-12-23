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
    ) where

import DeltaQ.Class
    ( DeltaQ (..)
    , Eventually (..)
    , Outcome (..)
    , eventually
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
