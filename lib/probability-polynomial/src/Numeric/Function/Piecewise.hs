{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

{-|
Copyright   : (c) Predictable Network Solutions Ltd., 2024
License     : BSD-3-Clause
Maintainer  : peter.thompson@pnsol.com
Description : Piecewise functions on the number line. 

-}
module Numeric.Function.Piecewise
    ( -- * Type
      Piecewise

      -- * Basic operations
    , zero
    , fromInterval
    , fromAscPieces
    , toAscPieces
    , intervals

      -- * Structure
    , mapPieces
    , mergeBy
    , trim

      -- * Numerical
    , evaluate
    , translateWith

      -- * Zip
    , zipPointwise
    ) where

import qualified Data.Function.Class as Fun

{-----------------------------------------------------------------------------
    Type
------------------------------------------------------------------------------}
-- | Internal representation of a single piece,
-- starting at a basepoint of type @a@
-- and containing an object of type @o@.
data Piece a o = Piece
    { basepoint :: a
    , object :: o
    }
    deriving (Eq, Show)

{- | A function defined piecewise on numerical intervals.
 
* @a@ = numerical type for the number line, e.g. 'Rational' or 'Double'
* @o@ = type of function on every piece
    e.g. polynomials or other specialized representations of functions

A value @f :: Piecewise a o@ represents a function

> eval f x = { 0           if -∞ <  x < x1
>            { eval o1 x   if x1 <= x < x2
>            { eval o2 x   if x2 <= x < x3
>            { …
>            { eval on x   if xn <= x < +∞

where @x1, …, xn@ are points on the real number line
(in strictly increasing order)
and where @o1, …, on@ are specialized representations functions,
e.g. polynomials.

In other words, the value @f@ represents a function that
is defined piecewise on half-open intervals.

The function 'intervals' returns the half-open intervals in the middle:

> intervals f = [(x1,x2), (x2,x3), …, (xn-1, xn)]

No attempt is made to merge intervals if the piecewise objects are equal,
e.g. the situation @o1 == o2@ may occur.

-}
data Piecewise a o
    = Pieces [Piece a o]
    deriving (Show)

{-$Piecewise Invariants

* The empty list represents the zero function.
* The 'basepoint's are in strictly increasing order.
* The internal representation of the function mentioned in the definition is

    > f = Pieces [Piece x1 o1, Piece x2 o2, …, Piece xn on]
-}

{-----------------------------------------------------------------------------
    Operations
------------------------------------------------------------------------------}
-- | The function which is zero everywhere.
zero :: Piecewise a o
zero = Pieces []

-- | @fromInterval (x1,x2) o@ creates a 'Piecewise' function
-- from a single function @o@ by restricting it to the
-- to half-open interval @x1 <= x < x2@.
-- The result is zero outside this interval.
fromInterval :: (Ord a, Num o) => (a,a) -> o -> Piecewise a o
fromInterval (x,y) o = Pieces [Piece start o, Piece end 0]
  where
    start = min x y
    end = max x y

-- | Build a piecewise function from an ascending list of contiguous pieces.
--
-- /The precondition (`map fst` of input list is ascending) is not checked./
fromAscPieces :: Ord a => [(a,o)] -> Piecewise a o
fromAscPieces = Pieces . map (uncurry Piece)

-- | Convert the piecewise function to a list of contiguous pieces
-- where the starting points of the pieces are in ascending order.
toAscPieces :: Ord a => Piecewise a o -> [(a,o)]
toAscPieces (Pieces xos) = [ (x, o) | Piece x o <- xos ]

-- | Intervals on which the piecewise function is defined, in sequence.
-- The last half-open interval, @xn <= x < +∞@, is omitted.
intervals :: Piecewise a o -> [(a,a)]
intervals (Pieces ys) =
    zip (map basepoint ys) (drop 1 $ map basepoint ys)

{-----------------------------------------------------------------------------
    Operations
    Structure
------------------------------------------------------------------------------}
-- | Map the objects of pieces.
mapPieces :: (o -> o') -> Piecewise a o -> Piecewise a o'
mapPieces f (Pieces ps) = Pieces [ Piece x (f o) | Piece x o <- ps ]

-- | Merge all adjacent pieces whose functions are considered
-- equal by the given predicate.
mergeBy :: Num o => (o -> o -> Bool) -> Piecewise a o -> Piecewise a o
mergeBy eq (Pieces pieces) = Pieces $ go 0 pieces
  where
    go _ [] = []
    go before (p : ps)
        | before `eq` object p = go before ps
        | otherwise = p : go (object p) ps

-- | Merge all adjacent pieces whose functions are equal according to '(==)'.
trim :: (Eq o, Num o) => Piecewise a o -> Piecewise a o
trim = mergeBy (==)

{-----------------------------------------------------------------------------
    Operations
    Evaluation
------------------------------------------------------------------------------}
{-|
Evaluate a piecewise function at a point.

> eval :: (Fun.Function o, Num o, Ord a, Num (Codomain o))
>         => Piecewise a o -> a -> Codomain o
-}
instance (Fun.Function o, Num o, Ord a, a ~ Fun.Domain o, Num (Fun.Codomain o))
    => Fun.Function (Piecewise a o)
  where
    type instance Domain (Piecewise a o) = a
    type instance Codomain (Piecewise a o) = Fun.Codomain o
    eval = evaluate

-- | Evaluate the piecewise function at a point.
-- See 'Piecewise' for the semantics.
evaluate
    :: (Fun.Function o, Num o, Ord a, Num (Fun.Codomain o), a ~ Fun.Domain o)
    => Piecewise a o -> a -> Fun.Codomain o
evaluate (Pieces pieces) x = go 0 pieces
 where
    go before [] = Fun.eval before x
    go before (p:ps)
        | basepoint p <= x = go (object p) ps
        | otherwise = Fun.eval before x

-- | Translate a piecewise function,
-- given a way to translate each piece.
--
-- >  eval (translate' y o) = eval o (x - y)
-- >    implies
-- >    eval (translateWith translate' y p) = eval p (x - y)
translateWith
    :: (Ord a, Num a, Num o)
    => (a -> o -> o)
    -> a -> Piecewise a o -> Piecewise a o
translateWith trans y (Pieces pieces) =
    Pieces [ Piece (x + y) (trans y o) | Piece x o <- pieces ]

{-----------------------------------------------------------------------------
    Operations
    Zip
------------------------------------------------------------------------------}
-- | Combine two piecewise functions by combining the pieces
-- with a pointwise operation that preserves @0@.
--
-- For example, `(+)` and `(*)` are pointwise operations on functions,
-- but convolution is not a pointwise operation.
--
-- Preconditions on the argument @f@:
--
-- * @f 0 0 = 0@
-- * @f@ is a pointwise operations on functions,
--   e.g. commutes with pointwise evaluation.
--
-- /The preconditions are not checked!/
zipPointwise
    :: (Ord a, Num o)
    => (o -> o -> o)
        -- ^ @f@
    -> Piecewise a o -> Piecewise a o -> Piecewise a o
zipPointwise f ps = mapPieces (uncurry f) . zipPieces ps

-- | Internal.
--
-- Combine two 'Piecewise' by pairing pieces on the same intervals.
-- It is usually necessary to split the intervals futher until
-- the intervals align exactly.
zipPieces
    :: (Ord a, Num b, Num c)
    => Piecewise a b -> Piecewise a c -> Piecewise a (b, c)
zipPieces (Pieces xs') (Pieces ys') =
    Pieces $ go 0 xs' 0 ys'
  where
    -- We split the intervals and combine the pieces in a single pass.
    --
    -- The algorithm is similar to mergesort:
    -- We walk both lists in parallel and generate a new piece by
    -- * taking the basepoint of the nearest piece
    -- * and combining it with the object that was overhanging from
    --   the previous piece (`xhang`, `yhang`)
    go _ [] _ [] = []
    go _ (Piece x ox : xstail) yhang [] =
        Piece x (ox, yhang) : go ox xstail yhang []
    go xhang [] _ (Piece y oy : ystail) =
        Piece y (xhang, oy) : go xhang [] oy ystail
    go xhang xs@(Piece x ox : xstail) yhang ys@(Piece y oy : ystail) =
        case compare x y of
            LT -> Piece x (ox, yhang) : go ox xstail yhang ys
            EQ -> Piece x (ox, oy)    : go ox xstail oy ystail
            GT -> Piece y (xhang, oy) : go xhang xs  oy ystail

{-----------------------------------------------------------------------------
    Operations
    Numeric
------------------------------------------------------------------------------}
{-| Algebraic operations '(+)', '(*)' and 'negate' on piecewise functions.

The functions 'abs' and 'signum' are defined using 'abs' and 'signum'
for every piece.

TODO: 'fromInteger' is __undefined__
-}
instance (Ord a, Num o) => Num (Piecewise a o) where
    (+) = zipPointwise (+)
    (*) = zipPointwise (*)
    negate = mapPieces negate
    abs = mapPieces abs
    signum = mapPieces signum
    fromInteger 0 = zero
    fromInteger n = error "TODO: fromInteger not implemented"
