{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-|
Description : Polynomials as lists of coefficients
Copyright   : (c) Peter Thompson, 2023
License     : BSD-2-Clause
Maintainer  : peter.thompson@pnsol.com
Stability   : experimental

We define a polynomial over a numeric type as a list of coefficients
of that type for increasing powers of the variable.
An empty list is not allowed: a zero polynomical must have at least one (zero) element.
-}
module Numeric.Polynomial.Simple
    ( -- * Basic operations
      Poly (..)
    , constant
    , zero
    , monomial
    , degree
    , shiftPolyUp
    , scale
    , eval
    , displayPoly

    -- * Advanced operations
    -- ** Algebraic
    , translate
    , integrate
    , differentiate
    , convolve

    -- ** Numerical
    , compareToZero
    , findPolyRoot
    ) where

{-----------------------------------------------------------------------------
    Basic operations
------------------------------------------------------------------------------}

-- | Polynomial with coefficients in @a@.
--
-- The list starts with the coefficient of lowest degree.
newtype Poly a = Poly [a]
    deriving (Show, Functor, Foldable)

instance Eq a => Eq (Poly a) where
    Poly x == Poly y = x == y

-- | The constant polynomial.
constant :: a -> Poly a
constant x = Poly [x]

-- | The zero polynomial.
zero :: Num a => Poly a
zero = constant 0

-- | Degree of a polynomial.
--
-- The degree of a constant polynomial is @0@, but
-- the degree of the zero polynomial is @-1@ for Euclidean division.
degree :: (Eq a, Num a) => Poly a -> Int
degree x = if trimPoly x == zero then -1 else length (trimPoly x) - 1

-- | remove top zeroes
trimPoly :: (Eq a, Num a) => Poly a -> Poly a
trimPoly (Poly as) = Poly (reverse $ goTrim $ reverse as)
  where
    goTrim [] = error "Empty polynomial"
    goTrim xss@[_] = xss -- can't use dropWhile as it would remove the last zero
    goTrim xss@(x : xs) = if x == 0 then goTrim xs else xss

-- | @monomial n a@ is the polynomial @a * x^n@.
monomial :: (Eq a, Num a) => Int -> a -> Poly a
monomial n x = if x == 0 then zero else Poly (reverse (x : replicate n 0))

-- | effectively multiply the polynomial by x by shifting all the coefficients up one place.
shiftPolyUp :: (Eq a, Num a) => Poly a -> Poly a
shiftPolyUp (Poly xs)
    | xs == [0] = Poly xs -- don't shift up zero
    | otherwise = Poly (0 : xs)

-- | Scale a polynomial by a scalar.
-- More efficient than multiplying by a constant polynomial.
scale :: Num a => a -> Poly a -> Poly a
scale x (Poly xs) = Poly (map (* x) xs)
    -- Does not agree with naming conventions in `Data.Poly`.

{-|
   Add polynomials by simply adding their coefficients as long as both lists continue.
   When one list runs out we take the tail of the longer list (this prevents us from just using zipWith!).
   Addtion might cancel out the highest order terms, so need to trim just in case.
-}
addPolys :: (Eq a, Num a) => Poly a -> Poly a -> Poly a
addPolys (Poly as) (Poly bs) = trimPoly (Poly (go as bs))
  where
    go [] ys = ys
    go xs [] = xs
    go (x : xs) (y : ys) = (x + y) : go xs ys

{-|
    multiply term-wise and then add (very simple - FFTs might be faster, but not for today)
    (a0 + a1x + a2x^2 + ...) * (b0 + b1x + b2x^2 ...)
    = a0 * (b0 + b1x + b2x^2 +...) + a1x * (b0 + b1x + ...)
    = (a0*b0) + (a0*b1x) + ...
              + (a1*b0x) +
                         + ...
    (may be an optimisation to be done by getting the shortest poly in the right place)
-}
mulPolys :: (Eq a, Num a) => Poly a -> Poly a -> Poly a
mulPolys as bs = sum (intermediateSums as bs)
  where
    intermediateSums :: (Eq a, Num a) => Poly a -> Poly a -> [Poly a]
    intermediateSums _ (Poly []) = error "Second polynomial was empty"
    intermediateSums (Poly []) _ = [] -- stop when we exhaust the first list
    -- as we consume the coeffecients of the first list, we shift up the second list to increase the power under consideration
    intermediateSums (Poly (x : xs)) ys =
        scale x ys : intermediateSums (Poly xs) (shiftPolyUp ys)

instance (Eq a, Num a) => Num (Poly a) where
    (+) = addPolys
    (*) = mulPolys
    negate (Poly a) = Poly (map negate a)
    abs = undefined
    signum = undefined
    fromInteger n = Poly [Prelude.fromInteger n]

{-|
Evaluate a polynomial at a point.

Uses Horner's method to minimise the number of multiplications.

@
a0 + a1·x + a2·x^2 + ... + a{n-1}·x^{n-1} + an·x^n
  = a0 + x·(a1 + x·(a2 + x·(… + x·(a{n-1} + x·an)) ))
@

-}
eval :: Num a => Poly a -> a -> a
eval (Poly as) x = foldr (\ai result -> x * result + ai) 0 as

{-| Create a given uniform spacing s over a range (l, u) return a list of (x, y) values of poly p over that range
First point will be at the base of the range, and then we increment the bottom of the interval by s
until it reaches the top of the interval
-}
displayPoly :: (Ord a, Eq a, Num a) => Poly a -> (a, a) -> a -> [(a, a)]
displayPoly p (l, u) s
    | s == 0 = [(l, eval p l)]
    | otherwise = goDisplay l
  where
    goDisplay x = if x >= u then [] else (x, eval p x) : goDisplay (x + s)

{-----------------------------------------------------------------------------
    Advanced Operations
------------------------------------------------------------------------------}

-- | Indefinite integral of a polynomial with constant term zero.
--
-- The integral of @x^n@ is @1/(n+1)·x^(n+1)@.
--
-- > eval (integrate p) 0 = 0
-- > integrate (differentiate p) = p - constant (eval p 0)
integrate :: (Eq a, Fractional a) => Poly a -> Poly a
integrate (Poly as) =
    -- Integrate by puting a zero constant term at the bottom and
    -- converting a x^n into a/(n+1) x^(n+1).
    -- 0 -> 0x is the first non-constant term, so we start at 1.
    -- When integrating a zero polynomial with a zero constant
    -- we get [0,0] so need to trim
    trimPoly (Poly (0 : zipWith (/) as (iterate (+ 1) 1)))

-- | Differentiate a polynomial.
--
-- We have @dx^n/dx = n·x^(n-1)@.
--
-- > differentiate (integrate p) = p
-- > differentiate (p * q) = (differentiate p) * q + p * (differentiate q)
differentiate :: Num a => Poly a -> Poly a
differentiate (Poly []) = error "Polynomial was empty"
differentiate (Poly [_]) = zero -- constant differentiates to zero
differentiate (Poly (_ : as)) =
    -- discard the constant term, everything else noves down one
    Poly (zipWith (*) as (iterate (+ 1) 1))

{-|
    Binomial coefficients: simple definition is n `choose` k ~ factorial n `div` (factorial k * factorial (n-k))
    Faster implementation available in math.combinatorics.exact.binomial
-}
choose :: Int -> Int -> Int
n `choose` k
    | k <= 0 = 1
    | k >= n = 1
    | otherwise = (n - 1) `choose` (k - 1) + (n - 1) `choose` k -- recursive definition

-- | Convolution of two polynomials defined on bounded intervals.
-- Produces three contiguous pieces as a result.
convolve
    :: (Fractional a, Eq a, Ord a) => (a, a, Poly a) -> (a, a, Poly a) -> [(a, Poly a)]
convolve (lf, uf, Poly fs) (lg, ug, Poly gs)
    | (lf < 0) || (lg < 0) = error "Interval bounds cannot be negative"
    | (lf >= uf) || (lg >= ug) = error "Invalid interval" -- upper bounds should be strictly greater than lower bounds
    | (ug - lg) > (uf - lf) = convolve (lg, ug, Poly gs) (lf, uf, Poly fs) -- if g is wider than f, swap the terms
    | otherwise -- we know g is narrower than f
        =
        let
            -- sum a set of terms depending on an iterator k (assumed to go down to 0), where each term is a k-dependent
            -- polynomial with a k-dependent multiplier
            sumSeries k mulFactor poly = sum [mulFactor n `scale` poly n | n <- [0 .. k]]

            -- the inner summation has a similar structure each time
            innerSum m n term k = sumSeries (m + k + 1) innerMult (\j -> monomial (m + n + 1 - j) (term j))
              where
                innerMult j =
                    fromIntegral
                        (if even j then (m + k + 1) `choose` j else negate ((m + k + 1) `choose` j))

            convolveMonomials m n innerTerm = sumSeries n (multiplier m n) (innerTerm m n)
              where
                multiplier p q k =
                    fromIntegral (if even k then q `choose` k else negate (q `choose` k))
                        / fromIntegral (p + k + 1)

            {-
                For each term, clock through the powers of each polynomial to give convolutions of monomials, which we sum.
                We extract each coefficient of each polynomial, together with an integer recording their position (i.e. power of x),
                and multiply the coefficients together with the new polynomial generated by convolving the monomials.
            -}
            makeTerm f =
                sum
                    [ (a * b) `scale` convolveMonomials m n f
                    | (m, a) <- zip [0 ..] fs
                    , (n, b) <- zip [0 ..] gs
                    ]

            firstTerm = makeTerm (\m n k -> innerSum m n (lg ^) k - monomial (n - k) (lf ^ (m + k + 1)))

            secondTerm = makeTerm (\m n -> innerSum m n (\k -> lg ^ k - ug ^ k))

            thirdTerm = makeTerm (\m n k -> monomial (n - k) (uf ^ (m + k + 1)) - innerSum m n (ug ^) k)
        in
            {-
                When convolving distributions, both distributions will start at 0 and so there will always be a pair of intervals
                with lg = lf = 0, so we don't need to add an initial zero piece.
                We must have lf + lg < lf + ug due to initial interval validity check. However, it's possible that lf + ug = uf + lg, so
                we need to test for a redundant middle interval
            -}
            if lf + ug == uf + lg
                then [(lf + lg, firstTerm), (uf + lg, thirdTerm), (uf + ug, zero)]
                else
                    [ (lf + lg, firstTerm)
                    , (lf + ug, secondTerm)
                    , (uf + lg, thirdTerm)
                    , (uf + ug, zero)
                    ]

-- | Translate the argument of a polynomial by summing binomial expansions.
--
-- > eval (translate y p) x = eval p (x - y)
--
translate :: (Fractional a, Eq a, Num a) => a -> Poly a -> Poly a
translate s (Poly ps) = sum [b `scale` binomialExpansion n s | (n, b) <- zip [0 ..] ps]
  where
    -- the binomial expansion of each power of x is a new polynomial
    -- whose coefficients are the product of
    -- a binomial coefficient and the shift value raised to a reducing power
    binomialTerm :: Num a => a -> Int -> Int -> a
    binomialTerm y n k = fromIntegral (n `choose` k) * (-y) ^ (n - k)
    binomialExpansion :: Num a => Int -> a -> Poly a
    binomialExpansion n y = Poly (map (binomialTerm y n) [0 .. n])

{-|
We use Sturm's Theorem to count the number of roots of a polynomial in a given interval.
(See https://en.wikipedia.org/wiki/Sturm%27s_theorem)
Starting from polynomial p, construct the Sturm sequence p0, p1, . . ., where:
p0 = p
p1 = p′
pi+1 = −rem(pi−1, pi) for i > 1
where p′ is the derivative of p and rem(p, q) is the remainder of the Euclidian division of p by q.
The length of this sequence is at most the degree of p.
We define V(x) to be the number of sign variations in the sequence of numbers p0(x), p1(x), . . ..
Sturm’s theorem states that, if p is a square-free polynomial (one without repeated roots), then
R(l,r](p) = V (l) − V (r). This extends to non-square-free polynomials provided neither l nor r is a
multiple root of p (a circumstance we shall ignore)

We start from the tuple that emerges from disagregation.
-}
countPolyRoots :: (Fractional a, Eq a, Ord a) => (a, a, Poly a) -> Int
countPolyRoots (l, r, p) = case degree p of
    -- p is the zero polynomial, so it doesn't *cross* zero
    -1 -> 0
    -- p is a non-zero constant polynomial - no root
    0 -> 0
    -- p is a linear polynomial, which has a root iff it has a different sign at each end of the interval
    1 -> if eval p l * eval p r < 0 then 1 else 0
    -- p has degree 2 or more so we can construct the Sturm sequence
    _ -> signVariations (sturmSequence l p) - signVariations (sturmSequence r p)
  where
    signVariations :: (Fractional a, Eq a, Ord a) => [a] -> Int
    {-
    When c0, c1, c2, . . . ck is a finite sequence of real numbers, then a sign variation or sign change in the sequence
    is a pair of indices i < j such that cicj < 0, and either j = i + 1 or ck = 0 for all k such that i < k < j
    -}
    signVariations xs = length (filter (< 0) pairsMultiplied)
      where
        -- we implement the clause "ck = 0 for all k such that i < k < j" by removing zero elements
        zeroesRemoved = filter (/= 0) xs
        -- TODO: deal with all zero corner case
        pairsMultiplied = zipWith (*) zeroesRemoved (tail zeroesRemoved)
    sturmSequence :: (Fractional a, Eq a, Ord a) => a -> Poly a -> [a]
    sturmSequence x q = map (flip eval x) (doSeq [differentiate q, q])
      where
        doSeq :: (Fractional a, Eq a, Ord a) => [Poly a] -> [Poly a]
        {-
           Note that this is called with a list of length 2 and grows the list, so we don't need to match all cases
           Note that we build this backwards to avoid use of append, but this doesn't affect the number of
           sign variations so there's no need to reverse it.
        -}
        doSeq x'@(xI : xIminusOne : _) = if polyRemainder == zero then x' else doSeq (negate polyRemainder : x')
          where
            polyRemainder = snd (euclidianDivision (xIminusOne, xI))
        doSeq _ = error "List too short" -- prevent warning about missing cases

{-|
See https://en.wikipedia.org/wiki/Polynomial_greatest_common_divisor#Euclidean_division
Take a pair of polynomials a, b, and produce the quotient and remainder q and r s.t. a = bq + r
Input: a and b ≠ 0 two polynomials; Output: q, the quotient, and r, the remainder;
Pseudocode:
    Begin
        q := 0
        r := a
        d := deg(b)
        c := lc(b)
        while deg(r) >= d do
            s := lc(r)/c x^(deg(r)-d)
            q := q + s
            r := r − sb
        end do
        return (q, r)
    end
-}
euclidianDivision
    :: (Fractional a, Eq a, Ord a) => (Poly a, Poly a) -> (Poly a, Poly a)
euclidianDivision (pa, pb) =
    if pb == zero
        then error "Division by zero polynomial"
        else goDivide (zero, pa)
  where
    degB = degree pb
    leadingCoefficient :: Eq a => Poly a -> a -- coefficient of the highest power term of the poly
    leadingCoefficient (Poly x) = last x
    lcB = leadingCoefficient pb
    -- goDivide :: (Fractional a, Eq a, Ord a) => (Poly a, Poly a) -> (Poly a, Poly a)
    goDivide (q, r) = if degree r < degB then (q, r) else goDivide (q + s, r - s * pb)
      where
        s = monomial (degree r - degB) (leadingCoefficient r / lcB)

{-|
    We measure whether or not a polynomial is consistently above or below zero, or equals zero
    Need to consider special cases where there is a root at a boundary point
-}
compareToZero :: (Fractional a, Eq a, Ord a) => (a, a, Poly a) -> Maybe Ordering
compareToZero (l, u, p)
    | l >= u = error "Invalid interval"
    | p == zero = Just EQ
    | lower * upper < 0 = Nothing -- quick test to eliminate simple cases
    | countPolyRoots (l, u, p) > 0 = Nothing -- polynomial crosses zero
    -- since the polynomial has no roots, the comparison is detmined by the boundary values
    | lower == 0 = Just (compare upper lower)
    | upper == 0 = Just (compare lower upper)
    | lower > 0 = Just GT -- upper must also be > 0 due to the lack of roots
    | otherwise = Just LT -- upper and lower both < 0 due to the lack of roots
  where
    lower = eval p l
    upper = eval p u

{-|
This is only called when there is known to be a root in the given interval, so we simply have to find it.
We do this by repeatedly halving the interval in which the root must lie.
If degree p <=1 (poly is constant or linear) we treat these as special cases
-}
findPolyRoot
    :: (Fractional a, Eq a, Num a, Ord a) => a -> (a, a) -> Poly a -> Maybe a
findPolyRoot precision (l, u) p
    | precision <= 0 = error "Invalid precision value"
    | degp < 0 = Just l -- the poly is zero, so the whole interval is a root, so return the basepoint
    | degp == 0 = Nothing -- the poly is a non-zeo constant so no root is present
    | degp == 1 = Just (-(head ps / last ps)) -- p0 + p1x = 0 => x = -p0/p1
    | otherwise = Just (halveInterval precision l u pl pu)
  where
    Poly ps = p
    degp = degree p
    pu = eval p u
    pl = eval p l
    halveInterval eps x y px py =
        let
            width = y - x
            mid = x + width / 2
            pmid = eval p mid
        in
            -- when the interval is small enough, stop: the root is in this interval, so take the mid point
            if width <= eps
                then mid
                else -- otherwise, if the polynomial has different signs at the ends of the lower half, choose this

                    if px * pmid < 0
                        then halveInterval eps x mid px pmid
                        else -- otherwise choose the upper interval and continue
                            halveInterval eps mid y pmid py