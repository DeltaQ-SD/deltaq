{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant bracket" #-}

{-|
Module      : IRVs
Description : Operations on improper random variables
Copyright   : (c) Peter Thompson, 2024
License     : BSD-2-Clause
Maintainer  : peter.thompson@pnsol.com
Stability   : experimental

We build IRVs as an abstract datatype on top of piecewise polynomials and deltas.
IRVs may be represented as PDFs or CDFs. They are polymorphic in a constrained numeric type.

We provide functions to construct IRVs as uniform distributions on an interval; a delta at a point;
and a CDF from a list of point-pairs.

We provide functions to turn an IRV into a set of point-pairs representing either the PDF or CDF.

We define operators for convolution, first-to-finish, all-to-finish and weighted choice.
We also provide an operation to extract the probability mass (<= 1) and the support of the
distribution.

We provide an implementation of the partial order on IRVs.

We enable extraction of centiles and computation of moments.

Note that we can consistently represent 'bottom' using polynomials and 'top' by including deltas.
-}
module PWPs.IRVs
    ( IRV
    , makePDF
    , makeCDF
    , constructUniform
    , constructDelta
    , zeroPDF
    , constructCDF
    , constructLinearCDF
    , constructGeneralCDF
    , firstToFinish
    , multiFtF
    , allToFinish
    , multiAtF
    , probChoice
    , multiWeightedChoice
    , (PWPs.IRVs.<+>)
    , probMass
    , asDiscreteCDF
    , asDiscretePDF
    , compareIRVs
    , support
    , top
    , bottom
    , centiles
    , cumulativeMass
    , shiftIRV
    , Moments (..)
    , moments
    , complexityOfIRV
    )
where

import Numeric.Polynomial.Simple
    ( Poly
    )
import PWPs.Piecewise
import PWPs.PiecewiseClasses
import PWPs.PolyDeltas

import qualified Numeric.Polynomial.Simple as Poly

type DistD a = Pieces a (PolyDelta a)
type DistP a = Pieces a (Poly a)

data IRV a = PDF (DistD a) | CDF (DistP a)
    deriving (Eq, Show)

top :: (Ord a, Num a) => IRV a
top = PDF (makePieces [(0, D 1), (0, Pd 0)])

bottom :: (Ord a, Num a) => IRV a
bottom = CDF (makePieces [(0, 0)])

cumulativeMass
    :: ( Ord a
       , Enum a
       , Num a
       , Fractional a
       , Evaluable a (DistP a)
       , Integrable (DistD a) (DistP a)
       )
    => IRV a
    -> a
    -> a
cumulativeMass x p = evaluate p (makeCDF x)

-- | Make a delta and convolve with it
shiftIRV
    :: (Fractional a, Ord a, Num a, Enum a, Eq a, Differentiable (DistP a) (DistD a))
    => a
    -> IRV a
    -> IRV a
shiftIRV s x = constructDelta s PWPs.IRVs.<+> PDF (makePDF x)

-- | Force an IRV into a PDF by differentiating if necessary
makePDF
    :: (Fractional a, Ord a, Num a, Enum a, Eq a, Differentiable (DistP a) (DistD a))
    => IRV a
    -> DistD a
makePDF (PDF x) = x
makePDF (CDF x) = differentiatePieces x

-- | Force an IRV into a CDF by integrating if necessary
makeCDF
    :: (Fractional a, Ord a, Num a, Enum a, Eq a, Integrable (DistD a) (DistP a))
    => IRV a
    -> DistP a
makeCDF (CDF x) = x
-- assume PDFs are 0 at 0
makeCDF (PDF x) = integratePieces x

-- | Construct a PDF with uniform probability from 0 to the given value
constructUniform :: (Fractional a, Ord a, Num a, Enum a, Eq a) => a -> IRV a
constructUniform x =
    if x <= 0
        then error "Invalid interval"
        else PDF (makePieces [(0, Pd (Poly.constant (1 / x))), (x, Pd 0)])

-- | Construct a PDF that is a delta function at the given value
constructDelta :: (Fractional a, Ord a, Num a, Enum a, Eq a) => a -> IRV a
constructDelta x
    | x < 0 = error "Invalid value"
    | x == 0 = PDF (makePieces [(0, D 1), (0, Pd 0)])
    | otherwise = PDF (makePieces [(0, Pd 0), (x, D 1), (x, Pd 0)])

-- | PDF with zero value everywhere
zeroPDF :: (Fractional a, Ord a, Num a, Enum a, Eq a) => IRV a
zeroPDF = PDF (makePieces [(0, Pd 0)])

monotonicFromZero :: (Ord a, Num a) => [a] -> Bool
monotonicFromZero xs = if null xs then error "Empty list" else head xs == 0 && monotonic xs

repeatedPoint :: Eq a => [a] -> Bool
repeatedPoint as = and $ zipWith (==) as (tail as)

{-| Construct a CDF from a list of (time, probability) pairs, which start at (0,0) and are monotonic.
     If the next time is strictly greater than the previous one, construct a linear interpolation between
     the two probabilities. If the next time is the same as the previous one, construct a Heaviside step
     between the two probabilities.
-}
constructGeneralCDF
    :: (Fractional a, Ord a, Num a, Enum a, Eq a) => [(a, a)] -> IRV a
constructGeneralCDF xs
    | length xs < 2 = error "Insufficient points"
    | not (monotonicFromZero (map fst xs)) = error "Basepoints not monotonic"
    | not (monotonicFromZero probabilities) = error "Probabilities not monotonic"
    | last probabilities > 1 = error "Probability exceeds one"
    | otherwise = (CDF . mergePieces . makePieces) (goCDF (head xs) (tail xs))
  where
    probabilities = map snd xs
    goCDF
        :: (Fractional a, Ord a, Num a, Enum a, Eq a)
        => (a, a)
        -> [(a, a)]
        -> [(a, Poly a)]
    -- construct a constant poly from last point
    goCDF (bf, pf) [] = [(bf, Poly.constant pf)]
    goCDF (bm, pm) ((bn, pn) : ys) =
        if bm == bn
            then goCDF (bn, pn) ys
            else (bm, Poly.lineFromTo (bm, pm) (bn, pn)) : goCDF (bn, pn) ys

{-| Construct a CDF from a list of values, treating each new value as a step up from the one before, assuming we start at 0
| First interval is a zero polynomial: subsequent intervals have a constant polynomial.
-}
constructCDF :: (Fractional a, Ord a, Num a, Enum a, Eq a) => [(a, a)] -> IRV a
constructCDF xs
    | length xs < 2 = error "Insufficient points"
    | not (monotonicFromZero basepoints) = error "Basepoints not monotonic"
    | repeatedPoint basepoints = error "Repeated basepoint"
    | not (monotonicFromZero probabilities) = error "Probabilities not monotonic"
    | last probabilities > 1 = error "Probability exceeds one"
    | otherwise =
        (CDF . makePieces) (zip basepoints (map Poly.constant probabilities))
  where
    basepoints = map fst xs
    probabilities = map snd xs

-- | Construct a CDF from a list of values, interpolating linearly between each pair of points
constructLinearCDF
    :: (Fractional a, Ord a, Num a, Enum a, Eq a) => [(a, a)] -> IRV a
constructLinearCDF xs
    | length xs < 2 = error "Insufficient points"
    | not (monotonicFromZero basepoints) = error "Basepoints not monotonic"
    | repeatedPoint basepoints = error "Repeated basepoint"
    | not (monotonicFromZero probabilities) = error "Probabilities not monotonic"
    | 0 `elem` steps = error "Zero-width interval"
    | last probabilities > 1 = error "Probability exceeds one"
    | otherwise = (CDF . makePieces) (zip basepoints segments)
  where
    {-
        Each linear polynomial has the form y = sx + c, where s is given by the difference
        in successive probabilities divided by the difference in succesive basepoints.
        The constant c is fixed by the constraint that we need to pass through the point (xn,yn),
        so cn = yn - xnsn
    -}
    basepoints = map fst xs
    probabilities = map snd xs
    steps = zipWith (-) (tail basepoints) basepoints -- width of each interval
    stepUps = zipWith (-) (tail probabilities) probabilities -- increments in probabilities
    slopes = zipWith (/) stepUps steps -- slope of each segment
    segments =
        ( zipWith3 makeSegment probabilities basepoints slopes
            ++ [Poly.constant (last probabilities)]
        )
    makeSegment y x s = Poly.fromCoefficients [y - x * s, s]

{-| Return a sequence of (Left) (time, step base) (the lower value of the jump at that point)
     or (Right) a sequence of (Time, Probability). The sequence is monotonically increasing in Time.
-}
asDiscreteCDF
    :: forall a
     . (Fractional a, Ord a, Num a, Enum a, Eq a, Integrable (DistD a) (DistP a))
    => IRV a
    -> Int
    -> [Either (a, a) [(a, a)]]
asDiscreteCDF xirv n =
    if n <= 0
        then error "Invalid number of points"
        else processRepeatedPoints (displayPolyDeltaIntervals (makeCDF xirv) spacing)
  where
    width = snd (support xirv) - fst (support xirv)
    spacing = width / Prelude.fromIntegral n
    processRepeatedPoints :: [Either (a, a) [(a, a)]] -> [Either (a, a) [(a, a)]]
    processRepeatedPoints [] = [] -- stop when list is empty
    processRepeatedPoints [x] = [x] -- nothing to do when only one element left
    processRepeatedPoints (x : xs) =
        let
            y = head xs
        in
            case (x, y) of
                (Right x0, Right y0) ->
                    -- both are just lists of points, assumed to be non-empty
                    let
                        (xLastPoint, xLastValue) = last x0
                        (yFirstPoint, yFirstValue) = head y0
                        xMinusLastPoint = (Right . init) x0
                        yMinusFirstPoint = (Right . tail) y0
                    in
                        if xLastPoint /= yFirstPoint
                            then x : processRepeatedPoints xs -- no repeated point, so nothing to do
                            else
                                if xLastValue /= yFirstValue -- we have a jump, so report it as a Left
                                    then xMinusLastPoint : (Left (xLastPoint, xLastValue)) : processRepeatedPoints xs
                                    else
                                        if xs == [y] -- reached the end
                                            then x : [yMinusFirstPoint]
                                            else x : processRepeatedPoints (yMinusFirstPoint : tail xs)
                -- if we have Lefts, just pass them through
                (_, _) -> x : processRepeatedPoints xs

{-| Return a sequence of (Left) Impulse Probablity mass (equivalent to the
     integral of the Heaviside function at that point) or (Right) a sequence
     of Time and Probability Density. The sequence is monotonically increasing in Time.
-}
asDiscretePDF
    :: (Fractional a, Ord a, Num a, Enum a, Eq a, Differentiable (DistP a) (DistD a))
    => IRV a
    -> Int
    -> [Either (a, a) [(a, a)]]
asDiscretePDF x n =
    if n <= 0
        then error "Invalid number of points"
        else displayPolyDeltaIntervals (makePDF x) spacing
  where
    width = snd (support x) - fst (support x)
    spacing = width / Prelude.fromIntegral n

firstToFinish
    :: (Fractional a, Ord a, Num a, Enum a, Eq a, Integrable (DistD a) (DistP a))
    => IRV a
    -> IRV a
    -> IRV a
firstToFinish x y = CDF (cdfOfx + cdfOfy - (cdfOfx * cdfOfy))
  where
    cdfOfx = makeCDF x
    cdfOfy = makeCDF y

{-| Compute the first-to-finsh of a list of IRVs by multiplying inverse CDFs then inverting
If there is nothing finish the result is bottom
-}
multiFtF
    :: (Fractional a, Ord a, Num a, Enum a, Eq a, Integrable (DistD a) (DistP a))
    => [IRV a]
    -> IRV a
multiFtF [] = bottom
multiFtF [x] = x
-- now know we have at least two, so head and tail are safe
multiFtF xs = CDF $ invert $ foldr (*) (head icdfs) (tail icdfs)
  where
    invert = applyObject (-) (Poly.constant 1)
    icdfs = map (invert . makeCDF) xs

allToFinish
    :: (Fractional a, Ord a, Num a, Enum a, Eq a, Integrable (DistD a) (DistP a))
    => IRV a
    -> IRV a
    -> IRV a
allToFinish x y = CDF (makeCDF x * makeCDF y)

-- | Compute the last-to-finsh of a list of IRVs: if we have nothing to wait for the result is top
multiAtF
    :: (Fractional a, Ord a, Num a, Enum a, Eq a, Integrable (DistD a) (DistP a))
    => [IRV a]
    -> IRV a
multiAtF [] = top
multiAtF [x] = x
-- now know we have at least two, so head and tail are safe
multiAtF xs = CDF $ foldr (*) (head cdfs) (tail cdfs)
  where
    cdfs = map makeCDF xs

{-|
    The probability is for choosing the left branch.
    We can do this on either PDFs or CDFs; if we have CDFs deliver a CDF,
    if we have both PDFs or one of each, deliver a PDF.
-}
probChoice
    :: (Fractional a, Ord a, Num a, Enum a, Eq a, Differentiable (DistP a) (DistD a))
    => a
    -> IRV a
    -> IRV a
    -> IRV a
probChoice p x y =
    if (p < 0) || (p > 1)
        then error "Invalid probability value"
        else case (x, y) of
            (CDF a, CDF b) -> CDF ((p >< a) + ((1 - p) >< b))
            _ -> PDF ((p >< makePDF x) + ((1 - p) >< makePDF y))

multiWeightedChoice
    :: (Fractional a, Ord a, Num a, Enum a, Eq a, Differentiable (DistP a) (DistD a))
    => [(a, IRV a)]
    -> IRV a
-- If we have nothing to choose from we get nothing
multiWeightedChoice [] = bottom
-- we'll force everything into PDFs and deliver a PDF
multiWeightedChoice xs = PDF (sum (zipWith adjust weights pdfs))
  where
    weights = map fst xs -- :: [a]
    pdfs = map (makePDF . snd) xs -- :: [DistD a]
    adjust x y = (x / sum weights) >< y

infix 7 <+> -- same as *

-- | To convolve, force into PDFs and then invoke piecewise convolution
(<+>)
    :: (Fractional a, Ord a, Num a, Enum a, Eq a, Differentiable (DistP a) (DistD a))
    => IRV a
    -> IRV a
    -> IRV a
x <+> y = PDF (makePDF x PWPs.Piecewise.<+> makePDF y)

-- | Just extract the final value of the CDF
probMass
    :: (Fractional a, Ord a, Num a, Enum a, Eq a, Integrable (DistD a) (DistP a))
    => IRV a
    -> a
probMass = piecesFinalValue . makeCDF

{-|
    If the two IRVs are partially ordered, return an ordering, otherwise return Nothing.
-}
compareIRVs
    :: (Fractional a, Ord a, Num a, Enum a, Eq a, Integrable (DistD a) (DistP a))
    => IRV a
    -> IRV a
    -> Maybe Ordering
compareIRVs x y = comparePW (makeCDF x) (makeCDF y)

-- | return the first and last basepoints for which the value is significant
support :: (Eq a, Fractional a) => IRV a -> (a, a)
support (PDF x) = piecewiseSupport x
support (CDF x) = piecewiseSupport x

{-|
Given an ordered list of probabiity values, return the time at which each value is reached;
if it is never reached, return Nothing in that position.
Where there is a step up in the CDF that includes the centile probability, we report the time
for which the probability is exceeded.
Centiles are not computed exactly, but to a given precision, currently set to 1/1000th of the
support of the IRV.
-}
centiles
    :: ( Fractional a
       , Ord a
       , Num a
       , Enum a
       , Eq a
       , Evaluable a (Poly a)
       , Integrable (DistD a) (DistP a)
       )
    => [a]
    -> IRV a
    -> [Maybe a]
centiles probabilities dQ
    | null probabilities = error "Empty probability list"
    | not (monotonic probabilities) = error "Probabilities not monotonic"
    | last probabilities > 1 = error "Probability exceeds one"
    | otherwise = findCentiles (probMass dQ) probabilities (getPieces $ makeCDF dQ)
  where
    -- need to specify the precision with which we report centiles
    precisionFraction = 1000 -- arbitrary parameter!
    eps = (snd (support dQ) - fst (support dQ)) / precisionFraction
    -- findCentiles :: a' -> [a'] -> [Piece a' (PolyHeaviside a')] -> [Maybe a']
    -- consume the list of probabilities and build the list of centiles
    findCentiles _ [] _ = [] -- stop when we have run out of centiles to evaluate
    findCentiles maxProb (x : xs) y@[final] =
        -- if we have only one piece its object must be constant, so report its basepoint as the root,
        -- unless we are beyond the tangible mass, in whioch case report Nothing
        (if x > maxProb then Nothing else Just (basepoint final))
            : findCentiles maxProb xs y
    -- hereon we must have at least two pieces, so we can ask whether the value is in the range of the current interval,
    findCentiles maxProb cs@(x : xs) remaining@(first : rest@(second : _))
        | x > maxProb = Nothing : findCentiles maxProb xs remaining
        -- if the value is in the range of the current interval,
        | firstValue <= x && x <= secondValue =
            -- find the precise root, and carry on to the next centile
            Poly.root eps x (basepoint first, basepoint second) (object first)
                : findCentiles maxProb xs remaining
        -- otherwise no further centiles can be in this piece, so move on to the next piece
        -- having first checked for a jump between the pieces
        | secondValue <= x && x < nextValue
            = Just (basepoint second) : findCentiles maxProb xs remaining
        | otherwise = findCentiles maxProb cs rest
        where
            firstValue  = evaluate (basepoint first) (object first)
            secondValue = evaluate (basepoint second) (object first)
            nextValue   = evaluate (basepoint second) (object second)
    findCentiles _ _ _ = error "Unexpected centile case"

data Moments a = Moments
    { tangibleMass :: a
    , mean :: a
    , variance :: a
    , skew :: a
    , kurtosis :: a
    }
    deriving (Eq, Show)

-- | Compute the first five moments of a given distribution
moments
    :: forall a
     . ( Fractional a
       , Ord a
       , Num a
       , Enum a
       , Eq a
       , Integrable (DistD a) (DistP a)
       , Differentiable (DistP a) (DistD a)
       , Evaluable a (DistD a)
       , Evaluable a (DistP a)
       )
    => IRV a
    -> Moments a
moments f =
    Moments
        { tangibleMass = tau
        , mean = mu
        , variance = sigsq
        , skew = gamma
        , kurtosis = kappa
        }
  where
    -- Compute the definite integral of x^nf(x)dx
    xNIntegral n g = piecesFinalValue (integralOfxToTheNtimesFx n g)
      where
        integralOfxToTheNtimesFx :: Int -> IRV a -> DistP a
        integralOfxToTheNtimesFx n' f' = integratePieces (applyObject (*) (Pd $ Poly.monomial n' 1) $ makePDF f')
    tau = xNIntegral 0 f
    -- catch case of bottom, otherwise scale back up to a proper distribution
    fHat = if tau == 0 then f else PDF ((1 / tau) >< makePDF f)
    mu = xNIntegral 1 fHat
    muSq = mu * mu
    sigsq = xNIntegral 2 fHat - muSq
    {-sqRoot :: a -> a
    sqRoot x =
        let
            y :: Double
            y = toRational x
        in fromRational . toRational . sqrt y -}
    sigma = squareRoot sigsq
    -- if the variance is zero there can be no skewness
    gamma =
        if sigsq == 0
            then 0
            else (xNIntegral 3 fHat - 3 * mu * sigsq - mu * muSq) / (sigma * sigsq)
    -- The kurtosis is bounded below by the squared skewness plus 1
    kappa =
        if sigsq == 0
            then 1
            else
                ( xNIntegral 4 fHat
                    - 4 * mu * gamma * sigma * sigsq
                    - 6 * muSq * sigsq
                    - muSq * muSq
                )
                    / (sigsq * sigsq)
    -- use Heron's method to compute the square root
    squareRoot :: (Fractional a, Num a, Ord a) => a -> a
    squareRoot x
        | x < 0 = error "Negative square root input"
        | x == 0 = 0
        | otherwise = goRoot x0
      where
        precision = x / 10000
        x0 = x / 2 -- initial guess
        goRoot xi = if abs (x - xi * xi) <= precision then xi else goRoot ((xi + x / xi) / 2)

complexityOfIRV
    :: ( Fractional a
       , Ord a
       , Num a
       , Enum a
       , Differentiable (DistP a) (DistD a)
       , ComplexityMeasureable (PolyDelta a)
       )
    => IRV a
    -> Int
complexityOfIRV x = piecewiseComplexity (makePDF x)
