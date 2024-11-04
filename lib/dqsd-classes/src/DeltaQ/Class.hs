{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

{-|
Copyright:
    Neil Davies, 2024
    Predictable Network Solutions Ltd., 2024
License: BSD-3-Clause
Maintainer: neil.davies@pnsol.com
-}
module DeltaQ.Class
    ( -- * Outcome
      Outcome (..)
    , (.>>.)
    , (.\/.)
    , (./\.)

    -- * DeltaQ
    , Eventually (..)
    , DeltaQ (..)

    -- * Laws TODO
    -- $laws
    ) where

{-----------------------------------------------------------------------------
    Outcome
------------------------------------------------------------------------------}

-- | An 'Outcome' is the result of an activity that takes time,
-- such as a distributed computation, communication, bus ride, … .
--
-- 'Outcome's can be composed in sequence or in parallel.
class (Ord (Duration o), Num (Duration o)) => Outcome o where
    type Duration o

    -- | The outcome that never finishes.
    never :: o

    -- | The outcome that succeeds after waiting for a fixed amount of time.
    wait :: Duration o -> o

    -- | Sequential composition:
    --
    -- First run the outcome on the left,
    -- then run the outcome on the right.
    sequentially :: o -> o -> o

    -- | Parallel composition, first to finish:
    --
    -- Run two outcomes in parallel,
    -- finish as soon as any one of them finishes.
    firstToFinish :: o -> o -> o

    -- | Parallel composiiton, last to finish:
    --
    -- Run two outcomes in parallel,
    -- finish after all of them have finished.
    lastToFinish :: o -> o -> o

infixl 1 .>>. -- less tight
infixr 2 .\/.
infixr 3 ./\. -- more tight

-- | Infix operator synonym for 'sequentially'
(.>>.) :: Outcome o => o -> o -> o
(.>>.) = sequentially

-- | Infix operator synonym for 'firstToFinish'
(.\/.) :: Outcome o => o -> o -> o
(.\/.) = firstToFinish

-- | Infix operator synonym for 'lastToFinish'
(./\.) :: Outcome o => o -> o -> o
(./\.) = lastToFinish


{-----------------------------------------------------------------------------
    Eventually
------------------------------------------------------------------------------}
-- | 'Eventually' represents a value that either eventually occurs
-- or is eventually abandoned.
--
-- Similar to the 'Maybe' type, but with a different 'Ord' instance:
-- @Occurs x < Abandoned@ for all @x@.
--
data Eventually a
    = Occurs a
    | Abandoned
    deriving (Eq, Show)

-- | For all @x@, we have @Occurs x < Abandoned@.
instance Ord a => Ord (Eventually a) where
    compare Abandoned Abandoned = EQ
    compare Abandoned (Occurs _) = GT
    compare (Occurs _) Abandoned = LT
    compare (Occurs x) (Occurs y) = compare x y

instance Functor Eventually where
    fmap _ Abandoned = Abandoned
    fmap f (Occurs x) = Occurs (f x)

-- |
-- > Abandoned <*> _ = Abandoned
-- > _ <*> Abandoned = Abandoned
instance Applicative Eventually where
    pure = Occurs

    Abandoned <*> Abandoned = Abandoned
    Abandoned <*> (Occurs _) = Abandoned
    (Occurs _) <*> Abandoned = Abandoned
    (Occurs f) <*> (Occurs y) = Occurs (f y)

{-----------------------------------------------------------------------------
    DeltaQ
------------------------------------------------------------------------------}

-- | 'DeltaQ' — quality attenuation.
--
-- 'DeltaQ' is a probability distribution of time.
--
-- Specifically, 'DeltaQ' is the probability distribution
-- of finish times for an outcome.
class (Num (Probability o), Outcome o) => DeltaQ o where
    type Probability o

    -- | Left-biased random choice.
    --
    -- @choice p@ chooses the left argument with probablity @p@
    -- and the right argument with probability @(1-p)@.
    choice :: Probability o -> o -> o -> o

    -- | Uniform probability distribution on a time interval.
    uniform :: Duration o -> Duration o -> o

    -- | Probability of /not/ finishing.
    failure :: o -> Probability o

    -- | Probability of finishing within the given time.
    successBefore :: o -> Duration o -> Probability o

    -- | Given a probability @p@, return the smallest time @t@
    -- such that the probability of completing within that time
    -- is at least @p@.
    --
    -- Return 'Abandoned' if the given probability
    -- exceeds the probability of finishing.
    quantile :: Probability o -> o -> Eventually (Duration o)

    -- | The earliest finish time with non-zero probability.
    --
    -- Return 'Abandoned' if the outcome is 'never'.
    earliest :: o -> Eventually (Duration o)

    -- | The last finish time which still has non-zero probability to occur.
    --
    -- Return 'Abandoned' if arbitrarily late times are possible.
    deadline :: o -> Eventually (Duration o)

{-----------------------------------------------------------------------------
    Laws
------------------------------------------------------------------------------}

{-$laws

Laws that we expect to hold.

TODO: Turn into property tests.

> never .>>. x = never
> never ./\. x = never
> never .\/. x = x

> wait t .>>. wait s = wait (t+s)
> wait t ./\. wait s = wait (max t s)
> wait t .\/. wait s = wait (min t s)

> earliest never = Abandoned
> earliest (wait t) = Occurs t
> earliest (x .>>. y) = (+) <$> earliest x <*> earliest y
> earliest (x ./\. y) = max (earliest x) (earliest y)
> earliest (x .\/. y) = min (earliest x) (earliest y)

> deadline never = Abandoned
> deadline (wait t) = Occurs t
> deadline (x .>>. y) = (+) <$> deadline x <*> deadline y
> deadline (x ./\. y) = max (deadline x) (deadline y)
> deadline (x .\/. y) = min (deadline x) (deadline y)

> failure never = 1
> failure (wait t) = 0
> failure (x .>>. y) = 1 - (1 - failure x) * (1 - failure y)
> failure (x ./\. y) = 1 - (1 - failure x) * (1 - failure y)
> failure (x .\/. y) = failure x * failure y

> if x <= y
>   earliest (uniform x y) = x
>   deadline (uniform x y) = y
>   quantile p (uniform x y) = x + p * (y-x)

> p <= q  implies  quantile p o <= quantile q o

> …

-}