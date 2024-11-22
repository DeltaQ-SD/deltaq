{-|
Copyright   : (c) Predictable Network Solutions Ltd., 2024
License     : BSD-3-Clause
Maintainer  : peter.thompson@pnsol.com
Description : Discrete, finite signed measures on the number line.
-}
module Numeric.Measure.Discrete
    ( Discrete
    , fromMap
    , toMap
    , dirac
    , total
    , cumulative
    , add
    , convolve
    ) where

import Data.Map
    ( Map
    )

import qualified Data.Map.Strict as Map

{-----------------------------------------------------------------------------
    Type
------------------------------------------------------------------------------}
-- | A discrete, finite
-- [signed measure](https://en.wikipedia.org/wiki/Signed_measure)
-- on the number line.
newtype Discrete a = Discrete (Map a a)
    -- INVARIANT: All values are non-zero.
    deriving (Show)

-- | Internal.
-- Remove all zero values.
trim :: (Ord a, Num a) => Map a a -> Map a a
trim m = Map.filter (/= 0) m

-- | Two measures are equal if they yield the same measures on every set.
--
-- @mx == my@ implies @forall t. cumulative mx t == cumulative my t@.
instance (Ord a, Num a) => Eq (Discrete a) where
    Discrete mx == Discrete my = mx == my

{-----------------------------------------------------------------------------
    Operations
------------------------------------------------------------------------------}
-- | A
-- [Dirac measure](https://en.wikipedia.org/wiki/Dirac_measure)
-- at the given point @x@.
--
-- > total (dirac x m) = m 
dirac :: (Ord a, Num a) => a -> a -> Discrete a
dirac _ 0 = Discrete Map.empty
dirac x m = Discrete (Map.singleton x m)

-- | Construct a discrete measure
-- from a collection of points and their measures.
fromMap :: (Ord a, Num a) => Map a a -> Discrete a
fromMap = Discrete . trim

-- | Decompose the discrete measure into a collection of points
-- and their measures.
toMap :: Num a => Discrete a -> Map a a
toMap (Discrete m) = m

-- | The total of the measure applied to the set of real numbers.
total :: Num a => Discrete a -> a
total (Discrete m) = sum m

-- | @cumulative x@ is the measure of the interval $(-∞, x]$.
cumulative :: (Ord a, Num a) => Discrete a -> a -> a
cumulative (Discrete m) x = sum $ Map.takeWhileAntitone (<= x) m

-- | Add two measures.
--
-- > total (add mx my) = total mx + total my
add :: (Ord a, Num a) => Discrete a -> Discrete a -> Discrete a
add (Discrete mx) (Discrete my) =
    Discrete $ trim $ Map.unionWith (+) mx my

-- | Additive convolution of two measures.
--
-- > convolve (dirac x wx) (dirac y wy) = dirac (x + y) (wx * wy)
-- > convolve (add mx my) mz = add (convolve mx mz) (convolve my mz)
-- > convolve mx (add my mz) = add (convolve mx my) (convolve mx mz)
-- > total (convolve mx my) = total mx * total my
convolve :: (Ord a, Num a) => Discrete a -> Discrete a -> Discrete a
convolve (Discrete mx) (Discrete my) =
    Discrete $ trim $ Map.fromListWith (+)
        [ (x + y, wx * wy)
        | (x,wx) <- Map.toList mx
        , (y,wy) <- Map.toList my
        ]
