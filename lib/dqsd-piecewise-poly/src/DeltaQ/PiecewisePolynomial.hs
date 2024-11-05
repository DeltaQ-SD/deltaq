{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module DeltaQ.PiecewisePolynomial
    ( Durations (..)
    ) where

import DeltaQ.Class
    ( DeltaQ (..)
    , Eventually (..)
    , Outcome (..)
    )
import PWPs.IRVs
    ( IRV
    )
import qualified PWPs.IRVs as PWP

{-----------------------------------------------------------------------------
    Implementation
------------------------------------------------------------------------------}

-- | Probability distribution of durations.
newtype Durations time = Durations {unDurations :: IRV time}

-- | Helper function for lifting a binary operation.
lift2
    :: (IRV a -> IRV a -> IRV a)
    -> Durations a -> Durations a -> Durations a
lift2 f (Durations x) (Durations y) = Durations (f x y)

instance Outcome (Durations Double) where
    type Duration (Durations Double) = Double

    never = Durations PWP.bottom
    wait t = Durations $ PWP.constructDelta t
    sequentially = lift2 (PWP.<+>)
    firstToFinish = lift2 PWP.firstToFinish
    lastToFinish = lift2 PWP.allToFinish

instance DeltaQ (Durations Double) where
    type Probability (Durations Double) = Double

    choice p =
        lift2 (PWP.probChoice p)

    uniform a b =
        Durations $ PWP.shiftIRV start $ PWP.constructUniform (end - start)
      where
        start = min a b
        end = max a b

    successBefore o t
        | Occurs t < earliest o = 0
        | deadline o < Occurs t = 1
        | otherwise = PWP.cumulativeMass (unDurations o) t

    failure (Durations x) =
        1 - PWP.probMass x

    quantile p (Durations x) =
        eventuallyFromMaybe $ head $ PWP.centiles [p] x

    earliest = Occurs . fst . PWP.support . unDurations
        -- FIXME: 'earliest' on 'never' is wrong
    deadline = Occurs . snd . PWP.support . unDurations
        -- FIXME: 'deadline' on 'never' is wrong.

{-----------------------------------------------------------------------------
    Helper functions
------------------------------------------------------------------------------}
eventuallyFromMaybe :: Maybe a -> Eventually a
eventuallyFromMaybe Nothing = Abandoned
eventuallyFromMaybe (Just x) = Occurs x
