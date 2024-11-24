{-|
Copyright   : (c) Predictable Network Solutions Ltd., 2024
License     : BSD-3-Clause
Maintainer  : peter.thompson@pnsol.com
Description : Finite signed measures on the number line.
-}
module Numeric.Measure.Finite.Mixed
    ( -- * Type
      Measure
    , zero
    , dirac
    , uniform
    , total
    , support
    , distribution
    , fromDistribution

      -- * Numerical operations
    , add
    , scale
    , translate
    , convolve
    ) where

import Data.Function.Class
    ( Function (..)
    )
import Numeric.Function.Piecewise
    ( Piecewise
    )
import Numeric.Polynomial.Simple
    ( Poly
    )

import qualified Data.Map.Strict as Map
import qualified Numeric.Function.Piecewise as Piecewise
import qualified Numeric.Measure.Discrete as D
import qualified Numeric.Polynomial.Simple as Poly

{-----------------------------------------------------------------------------
    Type
------------------------------------------------------------------------------}
-- | A finite
-- [signed measure](https://en.wikipedia.org/wiki/Signed_measure)
-- on the number line.
newtype Measure a = Measure (Piecewise a (Poly a))
    -- INVARIANT: Adjacent pieces contain distinct objects.
    -- INVARIANT: The last piece is a constant polynomial,
    --            so that the measure is finite.
    deriving (Show)

-- | Construct a signed measure from its
-- [distribution function
-- ](https://en.wikipedia.org/wiki/Distribution_function_(measure_theory)).
--
-- Return 'Nothing' if the measure is not finite,
-- that is if the last piece of the piecewise function is not constant.
fromDistribution
    :: (Ord a, Num a)
    => Piecewise a (Poly a) -> Maybe (Measure a)
fromDistribution pieces
    | isEventuallyConstant pieces = Just $ Measure $ trim pieces
    | otherwise = Nothing

-- | Test whether a piecewise polynomial is consant as x -> ∞.
isEventuallyConstant :: (Ord a, Num a) => Piecewise a (Poly a) -> Bool
isEventuallyConstant pieces
    | null xpolys = True
    | otherwise = (<= 0) . Poly.degree . snd $ last xpolys
  where
    xpolys = Piecewise.toAscPieces pieces

-- | Internal.
-- Join all intervals whose polynomials are equal.
trim :: (Ord a, Num a) => Piecewise a (Poly a) -> Piecewise a (Poly a)
trim = Piecewise.trim

-- | Two measures are equal if they yield the same measures on every set.
--
-- > mx == my
-- >   implies
-- >   forall t. eval (distribution mx) t = eval (distribution my) t
instance (Ord a, Num a) => Eq (Measure a) where
    Measure mx == Measure my =
        Piecewise.toAscPieces mx == Piecewise.toAscPieces my

{-----------------------------------------------------------------------------
    Operations
------------------------------------------------------------------------------}
-- | The measure that assigns @0@ to every set.
zero :: Num a => Measure a
zero = Measure Piecewise.zero

-- | A
-- [Dirac measure](https://en.wikipedia.org/wiki/Dirac_measure)
-- at the given point @x@.
--
-- > total (dirac x m) = m 
dirac :: (Ord a, Num a) => a -> a -> Measure a
dirac _ 0 = zero
dirac x w = Measure $ Piecewise.fromAscPieces [(x, Poly.constant w)]

-- | The probability measure of a uniform probability distribution
-- in the interval $[x,y)$
--
-- > total (uniform x y) = 1
uniform :: (Ord a, Num a, Fractional a) => a -> a -> Measure a
uniform x y = Measure $ case compare x y of
    EQ -> Piecewise.fromAscPieces [(x, 1)]
    _  -> Piecewise.fromAscPieces [(low, poly), (high, 1)]
  where
    low = min x y
    high = max x y
    poly = Poly.lineFromTo (low, 0) (high, 1)

-- | The total of the measure applied to the set of real numbers.
total :: (Ord a, Num a) => Measure a -> a
total (Measure p) =
    case Piecewise.toAscPieces p of
        [] -> 0
        ps -> eval (snd (last ps)) 0

-- | The 'support' is the smallest closed, contiguous interval $[x,y]$
-- outside of which the measure is zero.
--
-- Returns 'Nothing' if the interval is empty.
support :: (Ord a, Num a) => Measure a -> Maybe (a, a)
support (Measure pieces) =
    case Piecewise.toAscPieces pieces of
        [] -> Nothing
        ps -> Just (fst $ head ps, fst $ last ps)

-- | @eval (distribution m) x@ is the measure of the interval $(-∞, x]$.
--
-- This is known as the [distribution function
-- ](https://en.wikipedia.org/wiki/Distribution_function_(measure_theory)).
distribution :: (Ord a, Num a) => Measure a -> Piecewise a (Poly a)
distribution (Measure p) = p

-- | Add two measures.
--
-- > total (add mx my) = total mx + total my
add :: (Ord a, Num a) => Measure a -> Measure a -> Measure a
add (Measure mx) (Measure my) =
    Measure $ trim $ Piecewise.zipPointwise (+) mx my

-- | Scale a measure by a constant.
--
-- > total (scale a mx) = a * total mx
scale :: (Ord a, Num a) => a -> Measure a -> Measure a
scale 0 (Measure _) = zero
scale x (Measure m) = Measure $ Piecewise.mapPieces (Poly.scale x) m

-- | Translate a measure along the number line.
--
-- > eval (distribution (translate y m)) x
-- >    = eval (distribution m) (x - y)
translate :: (Ord a, Num a, Fractional a) => a -> Measure a -> Measure a
translate y (Measure m) =
    Measure $ Piecewise.translateWith Poly.translate y m

{-----------------------------------------------------------------------------
    Operations
    Decomposition into continuous and discrete measures,
    needed for convolution.
------------------------------------------------------------------------------}
-- | Measure that is absolutely continuous
-- with respect to the Lebesgue measure,
-- Represented via its distribution function.
newtype Continuous a = Continuous { unContinuous :: Piecewise a (Poly a) }
    -- INVARIANT: The last piece is @Poly.constant p@ for some @p :: a@.

-- | Density function (Radon–Nikodym derivative) of an absolutely
-- continuous measure.
newtype Density a = Density (Piecewise a (Poly a))
    -- INVARIANT: The last piece is @Poly.constant 0@.

-- | Density function of an absolutely continuous measure.
toDensity
    :: (Ord a, Num a, Fractional a)
    => Continuous a -> Density a
toDensity = Density . Piecewise.mapPieces Poly.differentiate . unContinuous

-- | Decompose a mixed measure into
-- a continuous measure and a discrete measure.
-- See also [Lebesgue's decomposition theorem
-- ](https://en.wikipedia.org/wiki/Lebesgue%27s_decomposition_theorem)
decompose
    :: (Ord a, Num a, Fractional a)
    => Measure a -> (Continuous a, D.Discrete a)
decompose (Measure m) =
    ( Continuous $ trim $ Piecewise.fromAscPieces withoutJumps
    , D.fromMap $ Map.fromList jumps
    )
  where
    pieces = Piecewise.toAscPieces m

    withoutJumps =
        zipWith (\(x,o) j -> (x, o - Poly.constant j)) pieces totalJumps
    totalJumps = tail $ scanl (+) 0 $ map snd jumps

    jumps = go 0 pieces
      where
        go _ [] = []
        go prev ((x,o) : xos) =
            (x, Poly.eval o x - Poly.eval prev x) : go o xos

{-----------------------------------------------------------------------------
    Operations
    Convolution
------------------------------------------------------------------------------}
{-$ NOTE [Convolution]

In order to compute a convolution,
we convolve a density with the distribution function.

Let $f$ denote a density, which can be continuous or a Dirac delta.
Let $G$ denote a distribution function.
Let $H = f * G$ be the result of the convolution.
It can be shown that this is the distribution function of the
convolution of the densities, $h = f * g$.

The formula for convolution is

$ H(y) = ∫ f(y - x) G(x) dx = ∫ f (x) G(y - x) dx$.

When $f$ is a sum of delta functions, $f = Σ w_j delta_{x_j}(x)$,
this integral becomes ($y - x = x_j$ => $x = y - x_j$)

$ H(y) = Σ w_j G(y - x_j) $.

When $f$ is a piecewise polynomial, we can convolve the pieces.

When convolving with a distribution function, the final piece
will be a constant $g_n$ on the interval $[x_n,∞)$.
In this case, the convolution is given by

\[
H(y)
    = ∫ f (x) G(y - x) dx
    = ∫_{ -∞}^{y-x_n} f(x) g_n dx
    = g_n F(y-x_n)
\]

where $F$ is the distribution function of the density $f$.
-}

-- | Convolve a discrete measure with a mixed measure.
--
-- See NOTE [Convolution].
convolveDiscrete
    :: (Ord a, Num a, Fractional a)
    => D.Discrete a -> Measure a -> Measure a
convolveDiscrete f gg =
    foldr add zero
        [ scale w (translate x gg)
        | (x, w) <- Map.toAscList $ D.toMap f
        ]

-- | Convolve an absolutely continuous measure with a mixed measure.
--
-- See NOTE [Convolution].
convolveContinuous
    :: (Ord a, Num a, Fractional a)
    => Continuous a -> Measure a -> Measure a
convolveContinuous (Continuous ff) (Measure gg)
    | null ffpieces = zero
    | null ggpieces = zero
    | otherwise = Measure $ trim $ boundedConvolutions + lastConvolution
  where
    ffpieces = Piecewise.toAscPieces ff
    ggpieces = Piecewise.toAscPieces gg

    Density f = toDensity (Continuous ff)
    fpieces = Piecewise.toAscPieces f

    -- Pieces on the bounded intervals
    boundedPieces xos =
        zipWith (\(x,o) (y,_) -> (x, y, o)) xos (drop 1 xos)

    boundedConvolutions =
        sum $
            [ Piecewise.fromAscPieces (Poly.convolve fo ggo)
            | fo <- boundedPieces fpieces
            , ggo <- boundedPieces ggpieces
            ]

    (xlast, plast) = last ggpieces
    glast = case Poly.toCoefficients plast of
        [] -> 0
        (a0:_) -> a0
    lastConvolution =
        Piecewise.mapPieces (Poly.scale glast)
        $ Piecewise.translateWith Poly.translate xlast ff

-- | Additive convolution of two measures.
--
-- > convolve (dirac x wx) (dirac y wy) = dirac (x + y) (wx * wy)
-- > convolve (add mx my) mz = add (convolve mx mz) (convolve my mz)
-- > convolve mx (add my mz) = add (convolve mx my) (convolve mx mz)
-- > total (convolve mx my) = total mx * total my
convolve
    :: (Ord a, Num a, Fractional a)
    => Measure a -> Measure a -> Measure a
convolve mx my =
    add (convolveContinuous contx my) (convolveDiscrete deltax my)
  where
    (contx, deltax) = decompose mx
