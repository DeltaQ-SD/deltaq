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
module DeltaQ.PiecewisePolynomialNew
    ( DQ
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
        eventuallyFromMaybe $ error "TODO: quantile"

    earliest (DQ m) = eventuallyFromMaybe $ fmap fst $ Measure.support m

    deadline (DQ m)= eventuallyFromMaybe $ fmap snd $ Measure.support m

{-----------------------------------------------------------------------------
    Helper functions
------------------------------------------------------------------------------}
eventuallyFromMaybe :: Maybe a -> Eventually a
eventuallyFromMaybe Nothing = Abandoned
eventuallyFromMaybe (Just x) = Occurs x
