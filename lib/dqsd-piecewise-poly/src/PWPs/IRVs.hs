{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}

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
(
    IRV
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

import PWPs.Piecewise
import PWPs.PolyDeltas
import PWPs.PolyHeavisides
import PWPs.Calculus ()
import PWPs.SimplePolynomials (Poly (..), makePoly, makeMonomial)
import PWPs.PiecewiseClasses

type MyConstraints a = (Fractional a, Ord a, Num a, Enum a, Eq a)
type DistD a = Pieces a (PolyDelta a)
type DistH a = Pieces a (PolyHeaviside a)

data IRV a = PDF (DistD a) | CDF (DistH a)
    deriving (Eq, Show)

top :: (Ord a, Num a) => IRV a
top = PDF (makePieces [(0, D 1), (0, Pd 0)])

bottom :: (Ord a, Num a) => IRV a
bottom = CDF (makePieces [(0, Ph 0)])

cumulativeMass :: (Ord a, Enum a, Num a, Fractional a, Evaluable a (DistH a), Integrable (DistD a) (DistH a)) => IRV a -> a -> a
cumulativeMass x p = last $ evaluate p (makeCDF x)

shiftIRV :: (Fractional a, Ord a, Num a, Enum a, Eq a, Differentiable (DistH a) (DistD a)) => a -> IRV a -> IRV a
-- | Make a delta and convolve with it
shiftIRV s x = constructDelta s PWPs.IRVs.<+> PDF (makePDF x)

makePDF :: (Fractional a, Ord a, Num a, Enum a, Eq a, Differentiable (DistH a) (DistD a)) => IRV a -> DistD a
-- | Force an IRV into a PDF by differentiating if necessary
makePDF (PDF x) = x
makePDF (CDF x) = differentiate x

makeCDF :: (Fractional a, Ord a, Num a, Enum a, Eq a, Integrable (DistD a) (DistH a)) => IRV a -> DistH a
-- | Force an IRV into a CDF by integrating if necessary
makeCDF (CDF x) = x
-- assume PDFs are 0 at 0
makeCDF (PDF x) = integrate x

constructUniform :: MyConstraints a => a -> IRV a
-- | Construct a PDF with uniform probability from 0 to the given value
constructUniform x = if x <= 0 then error "Invalid interval"
                     else PDF (makePieces [(0, Pd (makePoly (1/x))), (x, Pd 0)])

constructDelta :: MyConstraints a => a -> IRV a
-- | Construct a PDF that is a delta function at the given value
constructDelta x
    | x < 0 = error "Invalid value"
    | x == 0 = PDF (makePieces [(0, D 1), (0, Pd 0)])
    | otherwise = PDF (makePieces [(0, Pd 0), (x, D 1), (x, Pd 0)])

-- | PDF with zero value everywhere
zeroPDF :: MyConstraints a => IRV a
zeroPDF = PDF (makePieces [(0, Pd 0)])

monotonicFromZero :: (Ord a, Num a) => [a] -> Bool
monotonicFromZero xs = if null xs then error "Empty list" else head xs == 0 && monotonic xs

repeatedPoint :: Eq a => [a] -> Bool
repeatedPoint as = and $ zipWith (==) as (tail as)

constructGeneralCDF :: MyConstraints a => [(a, a)] -> IRV a
{- | Construct a CDF from a list of (time, probability) pairs, which start at (0,0) and are monotonic.
     If the next time is strictly greater than the previous one, construct a linear interpolation between
     the two probabilities. If the next time is the same as the previous one, construct a Heaviside step
     between the two probabilities.
-}
constructGeneralCDF xs
    | length xs < 2                         = error "Insufficient points"
    | not (monotonicFromZero (map fst xs))  = error "Basepoints not monotonic"
    | not (monotonicFromZero probabilities) = error "Probabilities not monotonic"
    | last probabilities > 1                = error "Probability exceeds one"
    | otherwise = (CDF . mergePieces . makePieces) (goCDF (head xs) (tail xs))
        where
            probabilities = map snd xs
            goCDF :: MyConstraints a => (a,a) -> [(a,a)] ->  [(a, PolyHeaviside a)]
            -- construct a constant poly from last point
            goCDF (bf,pf) [] = [(bf, Ph (Poly [pf]))]
            goCDF (bm,pm) ((bn,pn):ys) =
                if bm == bn then (bm, H pm pn):goCDF (bn,pn) ys
                            else (bm, Ph (slopeUp (bm,pm) (bn,pn))):goCDF (bn,pn) ys
                where
                    {- 
                        Each linear polynomial has the form y = sx + c, where s is given by the difference
                        in successive probabilities divided by the difference in succesive basepoints.
                        The constant c is fixed by the constraint that we need to pass through the point (b0,p0),
                        so c = p0 - b0*s.
                        If the slope is zero we just have a constant polynomial.
                    -}
                    slopeUp (b0,p0) (b1,p1) = if s == 0 then Poly [p0] else Poly [p0 - b0 * s, s]
                        where
                            -- we know b1 /= b0 so the division is safe
                            s = (p1-p0)/(b1-b0)

constructCDF :: MyConstraints a => [(a, a)] -> IRV a
-- | Construct a CDF from a list of values, treating each new value as a step up from the one before, assuming we start at 0
-- | First interval is a zero polynomial: subsequent intervals start with a delta and then have a constant polynomial.
constructCDF xs
    | length xs < 2                         = error "Insufficient points"
    | not (monotonicFromZero basepoints)    = error "Basepoints not monotonic"
    | repeatedPoint basepoints              = error "Repeated basepoint"
    | not (monotonicFromZero probabilities) = error "Probabilities not monotonic"
    | last probabilities > 1                = error "Probability exceeds one"
    | otherwise = (CDF . makePieces) (interleave treads risers)
        where
            basepoints = map fst xs
            probabilities = map snd xs
            -- Each step up corresponds to a delta of the difference with the previous value
            makeStep (x, y) = H x y
            risers = zip (tail basepoints) (zipWith (curry makeStep) probabilities (tail probabilities) )
            treads = zip basepoints (map (Ph . makePoly) probabilities) -- always have one more tread than riser
            interleave :: [b] -> [b] -> [b]
            interleave [] _ = []
            interleave [x] [] = [x] -- keep the last tread
            interleave _ [] = []
            interleave (x':xs') (y':ys') = x':y':interleave xs' ys'

constructLinearCDF:: MyConstraints a => [(a, a)] -> IRV a
-- | Construct a CDF from a list of values, interpolating linearly between each pair of points
constructLinearCDF xs
    | length xs < 2                         = error "Insufficient points"
    | not (monotonicFromZero basepoints)    = error "Basepoints not monotonic"
    | repeatedPoint basepoints              = error "Repeated basepoint"
    | not (monotonicFromZero probabilities) = error "Probabilities not monotonic"
    | 0 `elem` steps                        = error "Zero-width interval"
    | last probabilities > 1                = error "Probability exceeds one"
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
            steps   = zipWith (-) (tail basepoints) basepoints          -- width of each interval
            stepUps = zipWith (-) (tail probabilities) probabilities    -- increments in probabilities
            slopes  = zipWith (/) stepUps steps                         -- slope of each segment
            segments  = map Ph (zipWith3 makeSegment probabilities basepoints slopes ++ [makePoly (last probabilities)])
            makeSegment y x s = Poly [y - x*s, s]

asDiscreteCDF :: MyConstraints a => IRV a -> Int -> [Either (a,a) [(a, a)]]
{- | Return a sequence of (Left) step base (the lower value of the Heaviside function at that point)
     or (Right) a sequence of Time and Probability. The sequence is monotonically increasing in Time.-}
asDiscreteCDF x n = if n <= 0 then error "Invalid number of points"
                              else displayPolyDeltaIntervals (makeCDF x) spacing
    where
        width = snd (support x) - fst (support x)
        spacing = width / Prelude.fromIntegral n

asDiscretePDF :: MyConstraints a => IRV a -> Int -> [Either (a,a) [(a, a)]]
{- | Return a sequence of (Left) Impulse Probablity mass (equivalent to the
     integral of the Heaviside function at that point) or (Right) a sequence
     of Time and Probability Density. The sequence is monotonically increasing in Time. -}
asDiscretePDF x n = if n <= 0 then error "Invalid number of points"
                              else displayPolyDeltaIntervals (makePDF x) spacing
    where
        width = snd (support x) - fst (support x)
        spacing = width / Prelude.fromIntegral n

firstToFinish :: MyConstraints a => IRV a -> IRV a -> IRV a
firstToFinish x y = CDF (cdfOfx + cdfOfy - (cdfOfx * cdfOfy))
    where
        cdfOfx = makeCDF x
        cdfOfy = makeCDF y

multiFtF :: MyConstraints a => [IRV a] -> IRV a
-- | Compute the first-to-finsh of a list of IRVs by multiplying inverse CDFs then inverting
-- If there is nothing finish the result is bottom
multiFtF [] = bottom
multiFtF [x] = x
-- now know we have at least two, so head and tail are safe
multiFtF xs = CDF $ invert $ foldr (*) (head icdfs) (tail icdfs)
    where
        invert = applyObject (-) (Ph $ makePoly 1)
        icdfs = map (invert . makeCDF) xs

allToFinish :: MyConstraints a => IRV a -> IRV a -> IRV a
allToFinish x y = CDF (makeCDF x * makeCDF y)

multiAtF :: MyConstraints a => [IRV a] -> IRV a
-- | Compute the last-to-finsh of a list of IRVs: if we have nothing to wait for the result is top
multiAtF [] = top
multiAtF [x] = x
-- now know we have at least two, so head and tail are safe
multiAtF xs = CDF $ foldr (*) (head cdfs) (tail cdfs)
    where
        cdfs = map makeCDF xs

probChoice :: MyConstraints a => a -> IRV a -> IRV a -> IRV a
{- | 
    The probability is for choosing the left branch.
    We can do this on either PDFs or CDFs; if we have CDFs deliver a CDF, 
    if we have both PDFs or one of each, deliver a PDF.
-}
probChoice p x y = if (p < 0) || (p > 1) then error "Invalid probability value" else
    case (x,y) of
        (CDF a, CDF b) -> CDF ((p >< a) + ((1 - p) >< b))
        _              -> PDF ((p >< makePDF x) + ((1 - p) >< makePDF y))

multiWeightedChoice :: MyConstraints a => [(a, IRV a)] -> IRV a
-- If we have nothing to choose from we get nothing
multiWeightedChoice [] = bottom
-- we'll force everything into PDFs and deliver a PDF
multiWeightedChoice xs = PDF (sum (zipWith adjust weights pdfs))
    where
        weights = map fst xs                -- :: [a]
        pdfs    = map (makePDF . snd) xs    -- :: [DistD a]
        adjust x y = (x / sum weights) >< y

-- | To convolve, force into PDFs and then invoke piecewise convolution
infix 7 <+> -- same as *
(<+>) :: MyConstraints a => IRV a -> IRV a -> IRV a
x <+> y = PDF (makePDF x PWPs.Piecewise.<+> makePDF y)

probMass :: MyConstraints a => IRV a -> a
-- | Just extract the final value of the CDF
probMass = piecesFinalValue . makeCDF

compareIRVs :: MyConstraints a => IRV a -> IRV a -> Maybe Ordering
{- | 
    If the two IRVs are partially ordered, return an ordering, otherwise return Nothing.
-}
compareIRVs x y = comparePW (makeCDF x) (makeCDF y)

support :: (Eq a, Fractional a) => IRV a -> (a, a)
-- | return the first and last basepoints for which the value is significant
support (PDF x) = piecewiseSupport x
support (CDF x) = piecewiseSupport x

centiles :: (Fractional a, Ord a, Num a, Enum a, Eq a, Evaluable a (PolyHeaviside a)) => [a] -> IRV a -> [Maybe a]
-- | Given an ordered list of probabiity values, return the times at which each value is reached; 
-- if it is never reached, return Nothing in that position
centiles probabilities dQ
    | null probabilities            = error "Empty probability list"
    | not (monotonic probabilities) = error "Probabilities not monotonic"
    | last probabilities > 1        = error "Probability exceeds one"
    | otherwise = findCentiles (probMass dQ) probabilities (getPieces $ makeCDF dQ)
        where
            -- need to specify the precision with which we report centiles
            precisionFraction = 1000 -- arbitrary parameter!
            eps = (snd (support dQ) - fst (support dQ)) / precisionFraction
            --findCentiles :: a' -> [a'] -> [Piece a' (PolyHeaviside a')] -> [Maybe a']
            -- consume the list of probabilities and build the list of centiles
            findCentiles _ [] _ = [] -- stop when we have run out of centiles to evaluate
            findCentiles maxProb (x:xs) y@[final] =
            -- if we have only one piece its object must be constant, so report its basepoint as the root,
            -- unless we are beyond the tangible mass, in whioch case report Nothing
                (if x > maxProb then Nothing else Just (basepoint final)) : findCentiles maxProb xs y
            -- hereon we must have at least two pieces, so we can ask whether the value is in the range of the current interval,
            findCentiles maxProb cs@(x : xs) remaining@(first : rest@(second : _))
                | x > maxProb = Nothing : findCentiles maxProb xs remaining
                -- if the value is in the range of the current interval,
                | firstValue <= x && x <= secondValue
                -- find the precise root, and carry on to the next centile
                    = polyHeavisideRoot eps x (basepoint first, basepoint second) (object first) : findCentiles maxProb xs remaining
                -- otherwise no further centiles can be in this piece, so move on to the next piece
                | otherwise = findCentiles maxProb cs rest
                where
                    firstValue = head $ evaluate (basepoint first) (object first)
                    secondValue = last $ evaluate (basepoint second) (object first)
            findCentiles _ _ _ = error "Unexpected centile case"

data Moments a = Moments
    {
        tangibleMass :: a
      , mean         :: a
      , variance     :: a
      , skew         :: a
      , kurtosis     :: a
    }
    deriving (Eq, Show)
moments :: (Fractional a, Ord a, Num a, Enum a, Eq a, Integrable (DistD a) (DistH a), Evaluable a (DistD a), Evaluable a (DistH a)) =>
            IRV a -> Moments a
-- | Compute the first five moments of a given distribution
moments f = Moments
    {
        tangibleMass = tau
      , mean         = mu
      , variance     = sigsq
      , skew         = gamma
      , kurtosis     = kappa
    }
    where
        -- Compute the definite integral of x^nf(x)dx 
        xNIntegral n g = piecesFinalValue (integralOfxToTheNtimesFx n g)
            where
                integralOfxToTheNtimesFx :: (Fractional a, Ord a, Num a, Enum a, Eq a) => Int -> IRV a -> DistH a
                integralOfxToTheNtimesFx n' f' = integrate (applyObject (*) (Pd $ makeMonomial n' 1) $ makePDF f')
        tau   = xNIntegral 0 f
        -- catch case of bottom, otherwise scale back up to a proper distribution
        fHat  = if tau == 0 then f else PDF ((1/tau) >< makePDF f)
        mu    = xNIntegral 1 fHat
        muSq  = mu * mu
        sigsq = xNIntegral 2 fHat - muSq
        {-sqRoot :: a -> a
        sqRoot x = 
            let
                y :: Double
                y = toRational x
            in fromRational . toRational . sqrt y -}
        sigma = squareRoot sigsq
        -- if the variance is zero there can be no skewness
        gamma = if sigsq == 0 then 0 else (xNIntegral 3 fHat - 3 * mu * sigsq - mu * muSq)/(sigma * sigsq)
        -- The kurtosis is bounded below by the squared skewness plus 1
        kappa = if sigsq == 0 then 1 else (xNIntegral 4 fHat - 4 * mu * gamma * sigma * sigsq - 6 * muSq * sigsq - muSq * muSq)/(sigsq * sigsq)
        -- use Heron's method to compute the square root
        squareRoot :: (Fractional a, Num a, Ord a) => a -> a
        squareRoot x
            | x < 0     = error "Negative square root input"
            | x == 0    = 0
            | otherwise = goRoot x0
                where
                    precision = x / 10000
                    x0 = x/2 -- initial guess
                    goRoot xi = if abs (x - xi * xi) <= precision then xi else goRoot ((xi + x / xi)/2)

complexityOfIRV :: (Fractional a, Ord a, Num a,  Enum a,  Differentiable (DistH a) (DistD a), ComplexityMeasureable (PolyDelta a)) => IRV a -> Int
complexityOfIRV x = piecewiseComplexity (makePDF x)
