{-|
Copyright   : (c) Predictable Network Solutions Ltd., 2024
License     : BSD-3-Clause
Maintainer  : peter.thompson@pnsol.com
Description : Piecewise functions on the number line. 

-}
module Numeric.Function.Piecewise
    ( Piecewise
    , zero
    , fromInterval
    , intervals
    ) where

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

-- | Internal.
-- Map the objects of pieces.
mapPieces :: (o -> o') -> Piecewise a o -> Piecewise a o'
mapPieces f (Pieces ps) = Pieces [ Piece x (f o) | Piece x o <- ps ]

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

-- | Intervals on which the piecewise function is defined, in sequence.
-- The last half-open interval, @xn <= x < +∞@, is omitted.
intervals :: Piecewise a o -> [(a,a)]
intervals (Pieces ys) =
    zip (map basepoint ys) (drop 1 $ map basepoint ys)

