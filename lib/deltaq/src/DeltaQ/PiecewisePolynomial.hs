{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

{-|
Copyright   : Predictable Network Solutions Ltd., 2024
License     : BSD-3-Clause
Maintainer  : neil.davies@pnsol.com
Description : Instances via piecewise polynomials.

@'DQ'@ is a probability distribution of completion time
using the numeric type @Rational@.
This type represents a mixed discrete / continuous probability distribution
where the continuous part is represented in terms of piecewise polynomials.
-}
module DeltaQ.PiecewisePolynomial
    ( DQ
    , fromPositiveMeasure
    ) where

import Data.Maybe
    ( fromMaybe
    )
import DeltaQ.Class
    ( DeltaQ (..)
    , Eventually (..)
    , Outcome (..)
    )
import Numeric.Function.Piecewise
    ( Piecewise
    )
import Numeric.Measure.Finite.Mixed
    ( Measure
    )
import Numeric.Polynomial.Simple
    ( Poly
    )

import qualified Data.Function.Class as Function
import qualified Numeric.Function.Piecewise as Piecewise
import qualified Numeric.Measure.Finite.Mixed as Measure
import qualified Numeric.Polynomial.Simple as Poly

{-----------------------------------------------------------------------------
    Implementation
------------------------------------------------------------------------------}

-- | Probability distribution of durations.
newtype DQ = DQ (Measure Rational)
    deriving (Eq, Show)

-- | Interpret a finite, signed 'Measure' as a probability distribution.
--
-- In order to admit an interpretation as probability, the measure needs
-- to be positive, if that is not the case, the function returns 'Nothing'.
fromPositiveMeasure :: Measure Rational -> Maybe DQ
fromPositiveMeasure m
    | Measure.isPositive m = Just (DQ m)
    | otherwise = Nothing

-- | Helper function for lifting a binary operation on distribution functions.
onDistribution2
    :: (a ~ Rational)
    => String
    -> (Piecewise a (Poly a) -> Piecewise a (Poly a) -> Piecewise a (Poly a))
    -> DQ -> DQ -> DQ
onDistribution2 err f (DQ mx) (DQ my) =
    DQ
    $ fromMaybe impossible
    $ Measure.fromDistribution
    $ f (Measure.distribution mx) (Measure.distribution my)
  where
    impossible = error $ "impossible: not a finite measure in " <> err

instance Outcome DQ where
    type Duration DQ = Rational

    never = DQ Measure.zero

    wait t = DQ $ Measure.dirac t 1

    sequentially (DQ mx) (DQ my) = DQ (Measure.convolve mx my)

    firstToFinish = onDistribution2 "firstToFinish" $ \x y -> x + y - x * y

    lastToFinish = onDistribution2 "lastToFinish" (*)

instance DeltaQ DQ where
    type Probability DQ = Rational

    choice p = onDistribution2 "choice" $ \x y ->
        scale p x + scale (1 - p) y
      where
        scale = Piecewise.mapPieces . Poly.scale

    uniform a = DQ . Measure.uniform a

    successWithin (DQ m) = Function.eval (Measure.distribution m)

    failure (DQ m) = 1 - Measure.total m

    quantile p (DQ m) =
        eventuallyFromMaybe
        $ quantileFromMonotone (Measure.distribution m) p

    earliest (DQ m) = eventuallyFromMaybe $ fmap fst $ Measure.support m

    deadline (DQ m)= eventuallyFromMaybe $ fmap snd $ Measure.support m

{-----------------------------------------------------------------------------
    Operations
    quantile
------------------------------------------------------------------------------}
-- | Helper type for segements of a piecewise functions.
data Segment a b
    = Jump a (b,b)
    | Polynomial (a,a) (b,b) (Poly a)
    | End a b

-- | Helper function that elaborates a piecewise function
-- into a list of segments.
toSegments :: (a ~ Rational) => Piecewise a (Poly a) -> [Segment a a]
toSegments = goJump 0 . Piecewise.toAscPieces
  where
    goJump _ [] = []
    goJump prev ((x1, o) : xos)
        | y1 - y0 > 0 = Jump x1 (y0, y1) : nexts
        | otherwise = nexts
      where
        y1 = Poly.eval o x1
        y0 = Poly.eval prev x1
        nexts = goPoly x1 y1 o xos

    goPoly x1 y1 o [] =
        End x1 y1 : goJump o []
    goPoly x1 y1 o xos@((x2, _) : _) =
        Polynomial (x1, x2) (y1, Poly.eval o x2) o : goJump o xos
        -- TODO: What about the case where y1 == y2, i.e. a constant Polynomial?

-- | Compute a quantile from a monotonically increasing function.
quantileFromMonotone :: (a ~ Rational) => Piecewise a (Poly a) -> a -> Maybe a
quantileFromMonotone pieces = findInSegments segments
  where
    segments = toSegments pieces

    findInSegments [] y
        | y == 0 = Just 0
        | otherwise = Nothing
    findInSegments (Jump x1 (y1, y2) : xys) y
        | y1 < y && y <= y2 = Just x1
        | otherwise = findInSegments xys y
    findInSegments (Polynomial (x1, x2) (y1, y2) o : xys) y
        | y1 < y && y <= y2 = Poly.root precision y (x1, x2) o
        | otherwise = findInSegments xys y
    findInSegments (End x1 y1 : _) y
        | y1 == y = Just x1
        | otherwise = Nothing

precision :: Rational
precision = 1 / 10^(10 :: Integer)

{-----------------------------------------------------------------------------
    Helper functions
------------------------------------------------------------------------------}
eventuallyFromMaybe :: Maybe a -> Eventually a
eventuallyFromMaybe Nothing = Abandoned
eventuallyFromMaybe (Just x) = Occurs x
