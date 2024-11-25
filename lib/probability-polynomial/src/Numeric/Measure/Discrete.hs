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
    , distribution
    , add
    , convolve
    ) where

import Data.List
    ( scanl'
    )
import Data.Map
    ( Map
    )
import Numeric.Function.Piecewise
    ( Piecewise
    )
import Numeric.Polynomial.Simple
    ( Poly
    )

import qualified Data.Map.Strict as Map
import qualified Numeric.Function.Piecewise as Piecewise
import qualified Numeric.Polynomial.Simple as Poly

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
-- > mx == my
-- >   implies
-- >   forall t. eval (distribution mx) t = eval (distribution my) t
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

-- | @eval (distribution m) x@ is the measure of the interval $(-∞, x]$.
--
-- This is known as the [distribution function
-- ](https://en.wikipedia.org/wiki/Distribution_function_%28measure_theory%29).
distribution :: (Ord a, Num a) => Discrete a -> Piecewise a (Poly a)
distribution (Discrete m) =
    Piecewise.fromAscPieces
    $ zipWith (\(x,_) s -> (x,Poly.constant s)) diracs steps
  where
    diracs = Map.toAscList m
    steps = tail $ scanl' (+) 0 $ map snd diracs

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
