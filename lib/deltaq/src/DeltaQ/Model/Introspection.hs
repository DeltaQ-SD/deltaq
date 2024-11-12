{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module DeltaQ.Model.Introspection
    ( DeltaQIntrospection (..)
    , Slazard (..)
    )
where

import Data.Maybe (isNothing)
import DeltaQ.Model.DeltaQ
    ( DeltaQ (..)
    , ProbabilityMass (..)
    )

{-| The slack \/ hazard - the difference between the point in time \/
 probability mass space and the given `DeltaQ`. `Slack` represents the
 reference point being achieved; `Hazard` represents the point not being
 achieved a measure of the degree of it being missed.
-}
data (DeltaQ irv) => Slazard irv
    = -- | the __slack__. Expressed in terms of both time and probability mass
      Slack (Time irv) (ProbMass irv)
    | -- | the __hazard__. Expressed in terms of probability mass and, if waiting
      --   would have worked, time.
      Hazard (Maybe (Time irv)) (ProbMass irv)

instance (DeltaQ irv, Show (Time irv), Show (ProbMass irv)) => Show (Slazard irv) where
    show (Slack t p) = "Slack (" ++ show t ++ ", " ++ show p ++ ")"
    show (Hazard t' p) = "Hazard (" ++ maybe "NEVER" show t' ++ ", " ++ show p ++ ")"

-- | Ability to extract internal detail of aspects of the expressions.
class (DeltaQ irv) => DeltaQIntrospection irv where
    -- | Extract the probability that the timeout would occur.
    probTimedout :: irv -> Time irv -> ProbMass irv

    -- | Extract the /slack/ (or /hazard/) for a single (time, probability) point
    --   - the degenerative QTA (Quantitative Timeliness Agreement)
    pointSlackHazard
        :: irv
        -> (Time irv, ProbMass irv)
        -> Slazard irv

    -- | Given two DeltaQ return the partial ordering between them
    partialOrdering :: irv -> irv -> Maybe Ordering

    -- here? Things that might inform a scheduler, for example

    probTimedout irv to = complement $ cumulativeMass irv to

    pointSlackHazard irv (t, p)
        -- There must exist a non-negative time difference.
        | dp >= 0 = Slack dt (fromMassModel dp)
        -- There is no upper bound on the time.
        | isNothing t' = Hazard Nothing (fromMassModel $ negate dp)
        | otherwise = Hazard (Just $ negate dt) (fromMassModel $ negate dp)
      where
        dp = toMassModel p' - toMassModel p
        dt = t - (maybe err id t')

        t' = centile irv p
        p' = cumulativeMass irv t

        err = error "pointSlackHazard: inconsistency"
