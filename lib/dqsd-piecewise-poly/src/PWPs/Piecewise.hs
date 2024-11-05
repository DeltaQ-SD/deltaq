{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-|
Module      : Piecewise
Description : Sequences of objects defined over contiguous intervals
Copyright   : (c) Peter Thompson, 2024
License     : BSD-2-Clause
Maintainer  : peter.thompson@pnsol.com
Stability   : experimental

We consider 'objects' (which may be polynomials or deltas) defined over numerical intervals,
assumed to be in increasing order, where we know how to combine objects
when the intervals are identical. When the intervals are not identical we construct the
overlaps so as to produce a combined object over a more complex shared set of intervals.

We assume a piecewise list is never empty.

We allow for zero-width intervals in order to accomodate delta functions. A zero-width
interval should *only* contain a delta, and *must* be followed by a non-zero interval.
A non-zero-width interval should *not* contain a delta.

When combining piecewise objects we allow that one set of intervals may finish before the other,
and assume that whatever the operation is will simply reproduce the tail of the longer sequence.
-}
module PWPs.Piecewise
    ( Pieces (..)
    , Piece (..)
    , makePieces
    , mergePieces
    , zero
    , (><)
    , (<+>)
    , integratePieces
    , piecesFinalValue
    , monotonic
    , comparePW
    , piecewiseSupport
    , applyObject
    , displayPolyDeltaIntervals
    , piecewiseComplexity
    ) where

import PWPs.PiecewiseClasses

data Piece a o = Piece
    { basepoint :: a
    , object :: o
    }
    deriving (Eq, Show)

instance Functor (Piece a) where
    -- fmap :: (o -> b) -> Piece a o -> Piece a b
    fmap f x = x{object = f (object x)}

makePiece :: (a, o) -> Piece a o -- useful to have a tuple that we can have a list of to map over
makePiece (x, y) = Piece{basepoint = x, object = y}

newtype Pieces a o = Pieces {getPieces :: [Piece a o]}
    deriving (Eq, Show)
instance Functor (Pieces a) where
    fmap f = Pieces . map (fmap f) . getPieces

{-|
    We run through a set of pieces, merging intervals whose objects are declared mergeable
-}
mergePieces :: Mergeable b => Pieces a b -> Pieces a b
mergePieces f = Pieces (doMerge (getPieces f))
  where
    doMerge :: Mergeable b => [Piece a b] -> [Piece a b]
    doMerge [] = [] -- can occur when we have merged the last two pieces
    doMerge [x] = [x] -- stop when there's nothing left to merge
    doMerge (x0 : x1 : xs) = case mergeObject (object x0) (object x1) of
        Nothing -> x0 : doMerge (x1 : xs) -- can't merge so just move on
        Just o -> doMerge ((Piece{basepoint = basepoint x0, object = o}) : xs) -- extend interval through second basepoint

combinePieces
    :: (Num a, Eq a, Ord a, Mergeable b, Mergeable c, Mergeable d)
    => (b -> c -> d)
    -> Pieces a b
    -> Pieces a c
    -> Pieces a d

{-|
    We apply a binary operation on primitive objects to a pair of piecewise objects by first:
    aligning the pieces and pairing their respective objects, then;
    applying the binary operation to the contained object pairs.
    Finally we check whether any successive objects are identical and, if so, merge their respective intervals.
-}
combinePieces f x y = mergePieces (fmap (uncurry f) (alignPieces x y))

{-|
    We align two piecewise objects by splitting the intervals of one or the other to obtain a consistent set,
    returning a single list containing pairs of the corresponding objects. 
    Both lists must initally contain at least one element. Each step either adds an element to one or other list,
    or removes an element from both (generating an element of the output list). 
    The process terminates when removing an element from both lists leaves them empty. 
    Different alignment cases between the basepoints of the two lists determine how the input lists are transformed,
    and when an element of the output list is produced (for brevity, cases with x and y swapped are omitted).

    Schematically, x = [(bx0,Ox0),(bx1,Ox1),...] y = [(bx0,Oy0),(by1,Oy1),...

    (1) Initial interval underhangs 
        bx0 ----Ox0---- bx1 --Ox1-- bx2             -> bx0 ----Ox0---- bx1 --Ox1-- bx2
                             by0 --Oy0--- by1       -> bx0 -------O-------- by0 --Oy0--- by1                     

    (2) Initial basepoints align, but next points do not
        bx0 --Ox0-- bx1 ---Ox1--- bx2 --            -> bx0 --Ox0-- bx1 ---Ox1--- bx2 --
        by0 ------Oy0------ by1 ----Oy1----by2 --   -> by0 --Oy0-- bx1-Oy0-by1 ----Oy1----by2 --

    (3) Initial and next basepoints align
        bx0 ----Ox0---- bx1 --Ox1-- bx2 --          -> bx1 --Ox1-- bx2 --       } Terminate when
        by0 ----Oy0---- by1 ----Oy1----by2 --       -> by1 ----Oy1----by2 --    } both empty
                 \|/             
        bx0 -(Ox0,Oy0): recursive case     

    (4) Final interval(s) overhangs 
        bx0 -----Ox0----- bx1                       -> bx0 -----Ox0----- bx1 
        by0 -Oy0                                    -> by0 -----Oy0----- bx1 
-}
alignPieces
    :: (Num a, Eq a, Ord a, Mergeable b, Mergeable c)
    => Pieces a b
    -> Pieces a c
    -> Pieces a (b, c)
alignPieces x' y' = Pieces (doAlign (getPieces x') (getPieces y'))
  where
    doAlign
        :: (Num a', Eq a', Ord a', Mergeable b', Mergeable c')
        => [Piece a' b']
        -> [Piece a' c']
        -> [Piece a' (b', c')]
    doAlign [] _ = error "Empty piece list" -- lists should never be empty
    doAlign _ [] = error "Empty piece list" -- lists should never be empty

    -- both lists are non-empty
    doAlign x@(x0@Piece {basepoint = bx0, object = ox0}:xs) y@(y0@Piece {basepoint = by0, object = oy0}:ys)
        | bx0 < by0 -- case (1)
            = doAlign x (makePiece (bx0, zero):y)
        | bx0 > by0 -- case (1')
            = doAlign (makePiece (by0, zero):x) y
        | null ys && null xs  -- case (3) -- terminating case
            = [makePiece (bx0, (ox0, oy0))]
        | null ys   -- case (4)
            = doAlign x [y0, makePiece (basepoint (head xs), zero)]
        | null xs   -- case (4')
            = doAlign [x0, makePiece (basepoint (head ys), zero)] y
        | otherwise = -- basepoints align, next points exist
            let
                x1 = head xs
                y1 = head ys
                bx1 = basepoint x1
                by1 = basepoint y1
            in
                case compare bx1 by1 of
                    LT -> doAlign x (y0:makePiece (bx1, oy0):ys) -- case (2)
                    GT -> doAlign (x0:makePiece (by1, ox0):xs) y -- case (2')
                    EQ -> makePiece (bx0, (ox0, oy0)):doAlign xs ys -- case (3)

-- | Check that a list of values is monotonic (not strict to allow deltas)
monotonic :: Ord a => [a] -> Bool
monotonic ys = and (zipWith (<=) ys (tail ys))

makePieces :: (Num a, Eq a, Ord a) => [(a, o)] -> Pieces a o
-- check the basepoints are in order
makePieces xs
    | null xs = error "Provided list was empty"
    | length xs == 1 = Pieces [makePiece (head xs)] -- deal with the single element list case
    | not (monotonic (map fst xs)) = error "Basepoints were not in order"
    | otherwise = Pieces (map makePiece xs)

instance (Num a, Eq a, Ord a, Mergeable b, Num b) => Num (Pieces a b) where
    (+) = combinePieces (+)
    (*) = combinePieces (*)
    negate = fmap negate
    abs = undefined
    signum = undefined
    fromInteger n = Pieces [Piece{basepoint = 0 :: a, object = fromInteger n}]

instance
    (Num a, Eq a, Ord a, Differentiable b c, Mergeable c, Evaluable a b)
    => Differentiable (Pieces a b) (Pieces a c)
    where
    -- \| Piecewise differentiation is straightforward: just differentiate all the objects
    --        Since constants all differentate to zero, it is worth checking whether pieces can be merged.
    --
    differentiate = mergePieces . fmap differentiate
instance
    (Num a, Eq a, Ord a, Integrable b c, Mergeable c, Evaluable a c)
    => Integrable (Pieces a b) (Pieces a c)
    where
    integrate = integratePieces

{-|
For piecewise integration we need to evaluate at the boundary points to make the pieces join up.
We need to pass the integrated object to the next interation so that it can be evaluated on the basepoint
and to recognise deltas and pass them through as well as evaluating them
-}
integratePieces
    :: (Num a, Eq a, Ord a, Integrable b c, Evaluable a c) => Pieces a b -> Pieces a c
integratePieces ps = Pieces (goInt 0 (disaggregate (getPieces ps)))
  where
    goInt
        :: (Num a, Eq a, Ord a, Integrable b c, Evaluable a c)
        => a
        -> [(a, a, b)]
        -> [Piece a c]
    goInt _ [] = [] -- stop when the list of pieces is empty
    {-
       We evaluate each integrated object at the initial and final points of the interval.
       We receive the integrated value from the end of the previous interval, and adjust the integrated object so
       that its initial value matches this.
       We add the new object to the ouput list and pass on the received integated value plus the evaluated integral
       at the end of the interval.
    -}
    goInt previousIntegral ((fp, lp, x) : xs) = integratedPiece : goInt newIntegral xs
      where
        integratedObject = integrate x
        -- evaluate always returns a non-empty list, so head and last are safe
        basepointValue = head $ evaluate fp integratedObject -- the integral at the start of the current interval
        finalValue = last $ evaluate lp integratedObject -- the integral at the end of the current interval
        offset = previousIntegral - basepointValue
        integratedPiece = makePiece (fp, boost offset integratedObject) -- correct the constant
        newIntegral = finalValue + offset

{-|
To evaluate a piecewise object at a point we need to find the interval the point is in,
(i.e. find i s.t. basepoint(i) <= p < basepoint(i+1)) and then evaluate the corresponding object.
Our point may be beyond the last basepoint, in which case we take the final value,
or it may be before the first basepoint, in which case we evaluate the presumed zero object (to 0).
-}
evaluateAtApoint :: (Num a, Ord a, Evaluable a b) => a -> Pieces a b -> [a]
evaluateAtApoint point as
    | null pas = error "Empty piece list"
    | point < basepoint (head pas) = [0]
    | otherwise = goEval point pas
  where
    pas = getPieces as
    goEval _ [] = error "Empty piece list"
    goEval _ [_] = evaluate (basepoint (last pas)) (object (last pas))
    goEval p (x1 : x2 : xs) =
        if (basepoint x1 <= p) && (p < basepoint x2)
            then evaluate p (object x1)
            else goEval p (x2 : xs)

-- | Find the value of the ultimate object at the last basepoint
piecesFinalValue :: (Num a, Evaluable a b) => Pieces a b -> a
piecesFinalValue (Pieces []) = error "Empty piece list"
piecesFinalValue (Pieces xs) = last $ evaluate (basepoint (last xs)) (object (last xs))

instance (Num a, Eq a, Ord a, Evaluable a b) => Evaluable a (Pieces a b) where
    evaluate = evaluateAtApoint
    boost = fmap . boost
    scale = (><)

infix 7 <+>

{-|
Piecwise convolution requires convolving the pieces pairwise and then summing the results,
i.e. convolve every piece with every other piece and combine the results.
-}
(<+>)
    :: ( Ord a
       , Num a
       , Enum a
       , Fractional a
       , Num b
       , Mergeable b
       , Evaluable a b
       , CompactConvolvable a b
       )
    => Pieces a b
    -> Pieces a b
    -> Pieces a b
(<+>) (Pieces []) _ = error "Empty piece list"
(<+>) _ (Pieces []) = error "Empty piece list"
(<+>) as bs = sum [Pieces (map makePiece (convolveIntervals a b)) | a <- das, b <- dbs]
  where
    das = disaggregate (getPieces as)
    dbs = disaggregate (getPieces bs)

-- | disaggregate takes a Pieces list and produces a list of separate bounded intervals
disaggregate :: Num a => [Piece a o] -> [(a, a, o)]
disaggregate [] = error "Empty piece list"
disaggregate [x] = [(basepoint x, 1 + 2 * basepoint x, object x)] -- turn the last piece into an 'infinite' interval
disaggregate (x : xs@(x' : _)) = (basepoint x, basepoint x', object x) : disaggregate xs

displayPolyDeltaIntervals
    :: (Ord a, Enum a, Eq a, Fractional a, Num a, Displayable a b)
    => Pieces a b
    -> a
    -> [Either (a, a) [(a, a)]]
displayPolyDeltaIntervals as spacing = map (displayObject spacing) $ disaggregate (getPieces as)

infix 7 ><

-- | multiply by a constant piecewise
(><) :: (Eq a, Num a, Evaluable a b) => a -> Pieces a b -> Pieces a b
(><) = fmap . scale

-- | Check whether the pieces are all comparable, and if so if all compare the same way
comparePW
    :: (Fractional a, Eq a, Ord a, Comparable a b, Num b, Mergeable b, Evaluable a b)
    => Pieces a b
    -> Pieces a b
    -> Maybe Ordering
comparePW x' y' = goCompare (Just EQ) $ disaggregate $ getPieces $ alignPieces x' y'
  where
    goCompare
        :: (Fractional a, Eq a, Ord a, Comparable a b)
        => Maybe Ordering
        -> [(a, a, (b, b))]
        -> Maybe Ordering
    goCompare prev [] = prev -- when the list is exhauseted, keep the last result
    goCompare prev (x : xs)
        | prev == Just EQ = goCompare next xs -- Equality is neutral
        | next == Just EQ = goCompare prev xs -- Equality is neutral
        | prev == next = goCompare next xs -- The two intervals compare the same way, so carry on
        | otherwise = Nothing -- The two intervals compare differently, so stop
      where
        next = compareObjects x

-- | Return the aggregate interval over which the pieces are not zero
piecewiseSupport :: (Mergeable b, Eq b) => Pieces a b -> (a, a)
piecewiseSupport x = (start, end)
  where
    xs = getPieces x -- reduce to a list
    start =
        if length xs == 1
            then basepoint (head xs)
            else basepoint (head (dropWhile (\y -> object y == zero) xs))
    end = basepoint $ last xs

-- | Combine a single object with every object in a set of pieces
applyObject :: (b -> b -> b) -> b -> Pieces a b -> Pieces a b
applyObject f o = fmap (f o)

piecewiseComplexity :: ComplexityMeasureable b => Pieces a b -> Int
piecewiseComplexity ps = sum $ map (measureComplexity . object) $ getPieces ps
