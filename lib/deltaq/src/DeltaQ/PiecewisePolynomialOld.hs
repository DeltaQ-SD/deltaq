{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

{-|
Copyright:
    Predictable Network Solutions Ltd., 2024
License: BSD-3-Clause
Maintainer: neil.davies@pnsol.com
Description:
    Instances via piecewise polynomials.

@'Durations' a@ is a probability distribution of completion time
suing numeric type @a@.
This type represents a mixed discrete / continuous probability distribution
where the continuous part is represented in terms of piecewise polynomials.
-}
module DeltaQ.PiecewisePolynomialOld
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
    deriving (Show)

instance Eq (Durations Rational) where
    (Durations x) == (Durations y) =
        case PWP.compareIRVs x y of
            Just EQ -> True
            _ -> False

-- | Helper function for lifting a binary operation.
lift2
    :: (IRV a -> IRV a -> IRV a)
    -> Durations a -> Durations a -> Durations a
lift2 f (Durations x) (Durations y) = Durations (f x y)

instance Outcome (Durations Rational) where
    type Duration (Durations Rational) = Rational

    never = Durations PWP.bottom
    wait t = Durations $ PWP.constructDelta t
    sequentially = lift2 (PWP.<+>)
    firstToFinish = lift2 PWP.firstToFinish
    lastToFinish = lift2 PWP.allToFinish

instance DeltaQ (Durations Rational) where
    type Probability (Durations Rational) = Rational

    choice p =
        lift2 (PWP.probChoice p)

    uniform a b =
        Durations $ PWP.shiftIRV start $ PWP.constructUniform (end - start)
      where
        start = min a b
        end = max a b

    successWithin o t
        | Occurs t < earliest o = 0
        | deadline o <= Occurs t = 1
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
