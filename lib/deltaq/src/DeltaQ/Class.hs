{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

{-|
Copyright:
    Neil Davies, 2024
    Predictable Network Solutions Ltd., 2024
License: BSD-3-Clause
Maintainer: neil.davies@pnsol.com
Description:
    Type classes for outcomes and their completion times.

Type classes

* 'Outcome' — outcomes their combinations.
* 'DeltaQ' — probability distributions of completion times.

-}
module DeltaQ.Class
    ( -- * Type classes
      -- ** Outcome
      Outcome (..)

    -- ** DeltaQ
    , Eventually (..)
    , eventuallyFromMaybe

    , DeltaQ (..)

    -- * Properties
    -- $properties

    -- ** Outcome
    -- $properties-outcome

    -- ** DeltaQ
    -- $properties-deltaq
    ) where

{-----------------------------------------------------------------------------
    Outcome
------------------------------------------------------------------------------}

infixl 1 .>>. -- less tight
infixr 2 .\/.
infixr 3 ./\. -- more tight

-- | An 'Outcome' is the result of an activity that takes time,
-- such as a distributed computation, communication, bus ride, … .
--
-- 'Outcome's can be composed in sequence or in parallel.
class (Ord (Duration o), Num (Duration o)) => Outcome o where
    -- | Numerical type representing times in $[0,+∞)$.
    --
    -- For example 'Double' or 'Rational'.
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

    -- | Infix operator synonym for 'sequentially'.
    (.>>.) :: o -> o -> o
    (.>>.) = sequentially

    -- | Parallel composition, first to finish:
    --
    -- Run two outcomes in parallel,
    -- finish as soon as any one of them finishes.
    firstToFinish :: o -> o -> o

    -- | Infix operator synonym for 'firstToFinish'.
    (.\/.) :: o -> o -> o
    (.\/.) = firstToFinish

    -- | Parallel composiiton, last to finish:
    --
    -- Run two outcomes in parallel,
    -- finish after all of them have finished.
    lastToFinish :: o -> o -> o

    -- | Infix operator synonym for 'lastToFinish'.
    (./\.) :: o -> o -> o
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

-- | Helper function that converts 'Maybe' to 'Eventually'.
eventuallyFromMaybe :: Maybe a -> Eventually a
eventuallyFromMaybe Nothing = Abandoned
eventuallyFromMaybe (Just x) = Occurs x

{-----------------------------------------------------------------------------
    DeltaQ
------------------------------------------------------------------------------}

-- | 'DeltaQ' — quality attenuation.
--
-- 'DeltaQ' is a probability distribution of time.
--
-- Specifically, 'DeltaQ' is the probability distribution
-- of finish times for an outcome.
class   ( Eq (Probability o)
        , Enum (Probability o)
        , Num (Probability o)
        , Fractional (Probability o)
        , Outcome o
        )
    => DeltaQ o
  where
    -- | Numerical type representing probabilities in $[0,1]$.
    --
    -- For example 'Double' or 'Rational'.
    type Probability o

    -- | Left-biased random choice.
    --
    -- @choice p@ chooses the left argument with probablity @p@
    -- and the right argument with probability @(1-p)@.
    choice :: Probability o -> o -> o -> o

    -- | Random choice between multiple alternatives
    --
    -- @choices [(w_1, o_1), (w_2, o_2), …]@ chooses randomly between multiple
    -- outcomes. The probability @p_i@ for choosing the outcome @o_i@ is
    -- determined by the weights as @p_i = w_i / (w_1 + w_2 + …)@.
    choices :: [(Probability o, o)] -> o
    choices [] = never
    choices wos =
        foldr (uncurry choice) never
        $ zipWith (\wtot (w, o) -> (w / wtot, o)) ws wos
      where
        ws = scanr1 (+) (map fst wos)

    -- | Uniform probability distribution on a time interval.
    uniform :: Duration o -> Duration o -> o

    -- | Probability of /not/ finishing.
    failure :: o -> Probability o

    -- | Probability of finishing within the given time @t@.
    --
    -- \"Within\" is inclusive,
    -- i.e. this returns the probability that the finishing time is @<= t@.
    successWithin :: o -> Duration o -> Probability o

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
    Properties
------------------------------------------------------------------------------}
{-$properties
All instances of the above type classes are expected to satisfy
the following properties.

For instances that use approximate arithmetic
such as floating point arithmetic or fixed precision arithmetic,
equality may be up to numerical accuracy.
-}

{-$properties-outcome

'never'

> never .>>. y = never
> never ./\. y = never
> never .\/. y = y
>
> x .>>. never = never
> x ./\. never = never
> x .\/. never = x

'wait'

> wait t .>>. wait s  =  wait (t+s)
> wait t ./\. wait s  =  wait (max t s)
> wait t .\/. wait s  =  wait (min t s)

'(.>>.)'

> (x .>>. y) .>>. z  =  x .>>. (y .>>. z)

'(./\.)'

> (x ./\. y) ./\. z  =  x ./\. (y ./\. z)
>
> x ./\. y  =  y ./\. x

'(.\/.)'

> (x .\/. y) .\/. z  =  x .\/. (y .\/. z)
>
> x .\/. y  =  y .\/. x

-}

{-$properties-deltaq

'choice'

> choice 1 x y = x
> choice 0 x y = y
>
> choice p x y .>>. z  =  choice p (x .>>. z) (y .>>. z)
> choice p x y ./\. z  =  choice p (x ./\. z) (y ./\. z)
> choice p x y .\/. z  =  choice p (x .\/. z) (y .\/. z)

'choices'

> choices [] = never
> choices ((w,o) : wos) = choice p o (choices wos)
>   where  p = w / (w + sum (map fst wos))

'uniform'

> wait t .>>. uniform r s  =  uniform (t+r) (t+s)
> uniform r s .>>. wait t  =  uniform (r+t) (s+t)

'failure'

> failure never      = 1
> failure (wait t)   = 0
> failure (x .>>. y) = 1 - (1 - failure x) * (1 - failure y)
> failure (x ./\. y) = 1 - (1 - failure x) * (1 - failure y)
> failure (x .\/. y) = failure x * failure y
>
> failure (choice p x y) = p * failure x + (1-p) * failure y
> failure (uniform r s)  = 0

'successWithin'

> successWithin never    t = 0
> successWithin (wait s) t = if t < s then 0 else 1
>
> successWithin (x ./\. y) t =
>   successWithin t x * successWithin t y
> successWithin (x .\/. y) t =
>   1 - (1 - successWithin t x) * (1 - successWithin t y)
>
> successWithin (choice p x y) t =
>   p * successWithin t x + (1-p) * successWithin t y
> successWithin (uniform r s) t
>   | t < r           = 0
>   | r <= t && t < s = (t-r) / (s-r)
>   | s <= t          = 1

'quantile'

> p <= q  implies  quantile p o <= quantile q o
>
> quantile p (uniform r s)  =  r + p*(s-t)  if r <= s

'earliest'

> earliest never      = Abandoned
> earliest (wait t)   = Occurs t
> earliest (x .>>. y) = (+) <$> earliest x <*> earliest y
> earliest (x ./\. y) = max (earliest x) (earliest y)
> earliest (x .\/. y) = min (earliest x) (earliest y)
>
> earliest (choice p x y) = min (earliest x) (earliest y)  if p ≠ 0, p ≠ 1
> earliest (uniform r s)  = Occurs r   if r <= s

'deadline'

> deadline never      = Abandoned
> deadline (wait t)   = Occurs t
> deadline (x .>>. y) = (+) <$> deadline x <*> deadline y
> deadline (x ./\. y) = max (deadline x) (deadline y)
> deadline (x .\/. y) = min (deadline x) (deadline y)
>  -- TODO: This property ^ is actually false!
>
> deadline (choice p x y) = max (deadline x) (deadline y)  if p ≠ 0, p ≠ 1
>  -- TODO: This property ^ is actually false!
>
> deadline (uniform r s)  = Occurs s   if r <= s

-}