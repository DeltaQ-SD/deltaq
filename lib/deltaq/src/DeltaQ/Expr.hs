{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

{-|
Copyright   :
    Predictable Network Solutions Ltd., 2020-2024
    PLWORKZ R&D, 2025
License     : BSD-3-Clause
Description : Outcome expressions.

The type 'O' represents outcome expressions that can be

* inspected,
* rendered as outcome diagrams, and
* converted to a probability distribution of completion times.

-}
module DeltaQ.Expr
    ( -- * Outcome expressions
      O
    , var
    , loc
    , substitute
    , choices'
    , toDeltaQ

    -- * Outcome terms
    , termFromOutcome
    , outcomeFromTerm
    , Term (..)
    , isVar
    , isLoc
    , isSeq
    , isLast
    , isFirst
    , isParallel
    , isChoices
    , maxParallel
    , everywhere
    , everything
    , isNormalizedAssoc
    , normalizeAssoc
    ) where

import Control.Monad
    ( ap
    )
import Control.DeepSeq
    ( NFData
    )
import Data.List
    ( foldl'
    )
import DeltaQ.Class
    ( Outcome (..)
    , DeltaQ (..)
    )
import DeltaQ.PiecewisePolynomial
    ( DQ
    )
import GHC.Generics
    ( Generic
    )

{-----------------------------------------------------------------------------
    O
------------------------------------------------------------------------------}
-- | Outcome expression.
--
-- This type includes graphical annotations for the rendering
-- as outcome diagram.
newtype O = O { unO :: Term String }
    -- INVARIANT: Terms satisfy 'isNormalizedAssoc'.
    deriving (Show, Generic, NFData)

-- | Outcome variable, given by a unique name.
var :: String -> O
var = O . Var

-- | Observation location, with a label.
--
-- Rendered as a box with a label in the outcome diagram.
-- Put this in sequence with other outcome expressions to highlight
-- their outcome locations.
loc :: String -> O
loc = O . Loc

-- | Substitute all variables by outcome expressions.
substitute
    :: (String -> O)
        -- ^ Assignment of variable names to outcome expressions.
    -> O
        -- ^ Outcome expression in which we substitute.
    -> O
substitute f (O term) = O $ normalize1Assoc $ term >>= unO . f

-- | Outcome expressions can be mapped to
-- probability distributions of completion times 'DQ',
-- provided that we know how to map 'var'.
toDeltaQ :: (String -> DQ) -> O -> DQ
toDeltaQ f (O term) = go term
  where
    go (Var v)  = f v
    go Never    = never
    go Wait0    = wait 0
    go (Wait t) = wait t
    go (Loc  _) = wait 0
    go (Seq   xs) = foldr1 (.>>.) $ map go xs
    go (Last  xs) = foldr1 (./\.) $ map go xs
    go (First xs) = foldr1 (.\/.) $ map go xs
    go (Choices wxs) = choices [ (w, go x) | (w, x) <- wxs ]

-- | Outcome expressions are instances of 'Outcome'.
instance Outcome O where
    type Duration O = Rational

    never = O Never
    wait = O . Wait
    sequentially  (O x) (O y) = O . normalize1Assoc $ Seq [x,y]
    firstToFinish (O x) (O y) = O . normalize1Assoc $ First [x,y]
    lastToFinish  (O x) (O y) = O . normalize1Assoc $ Last [x,y]

-- | Specialization of 'choices' for outcome expressions.
choices' :: [(Rational, O)] -> O
choices' wos =
    outcomeFromTerm $ Choices [ (w, termFromOutcome x) | (w, x) <- wos ]

{-----------------------------------------------------------------------------
    Terms
------------------------------------------------------------------------------}
-- | Term representation for outcome expressions.
--
-- Different terms may represent equal outcomes.
data Term v
    = Var v
        -- ^ Variable, to be substituted by a more concrete outcome.
    | Never
        -- ^ Outcome that never finishes.
    | Wait0
        -- ^ Succeed immediately. Equivalent to @Wait 0@,
        -- but with a straight line as graphical representation.
    | Wait Rational
        -- ^ Succeed after waiting for a fixed amount of time.
    | Loc String
        -- ^ Outcome location with a label.
        -- Equivalent to @Wait 0@, but with a labeled box
        -- as graphical representation.
    | Seq [Term v]
        -- ^ Sequential composition.
    | Last [Term v]
        -- ^ Parallel composition, last to finish.
    | First [Term v]
        -- ^ Parallel composiiton, last to finish.
    | Choices [(Rational, Term v)]
        -- ^ Probabilistic choice.
        -- The probabilities are proportional to the given weights.
    deriving (Show, Eq, Ord, Generic, Functor, NFData)

instance Applicative Term where
    pure = Var
    (<*>) = ap

-- | '(>>=)' is substitution.
instance Monad Term where
    m >>= g = go m
      where
        go (Var v)    = g v
        go Never      = Never
        go Wait0      = Wait0
        go (Wait t)   = Wait t
        go (Loc  s)   = Loc s
        go (Seq   xs) = Seq   $ map go xs
        go (Last  xs) = Last  $ map go xs
        go (First xs) = First $ map go xs
        go (Choices wxs) = Choices [ (w, go x) | (w, x) <- wxs ]

-- | Inspect an outcome expression 'O' through its 'Term' representation.
--
-- The result 'Term' satisfies 'isNormalizedAssoc'.
termFromOutcome :: O -> Term String
termFromOutcome (O term) = term

-- | Construct an outcome expression 'O' from an outcome 'Term'.
outcomeFromTerm :: Term String -> O
outcomeFromTerm = O . normalizeAssoc

-- | Predicate that defines the \"associative normal form\" for a 'Term'.
--
-- Specifically, a 'Term' is said to be in \"associative normal form\"
-- if the arguments to the constructors 'Seq', 'Last', and 'First'
--
-- * are nonempty lists, and
-- * their list elements do not have an outermost constructor of the same
--   kind, i.e. @Seq [Seq …, First …]@ is not allowed because
--   one of the list elements for a 'Seq' constructor
--   is also a 'Seq' constructor.
--
-- and if the argument to the constructor 'Choices'
--
-- * is a nonempty list
--
isNormalizedAssoc ::Term v -> Bool 
isNormalizedAssoc (Seq xs) =
    not (null xs)
    && all (not . isSeq) xs
    && all isNormalizedAssoc xs
isNormalizedAssoc (Last xs) =
    not (null xs)
    && all (not . isLast) xs
    && all isNormalizedAssoc xs
isNormalizedAssoc (First xs) =
    not (null xs)
    && all (not . isFirst) xs
    && all isNormalizedAssoc xs
isNormalizedAssoc (Choices wxs) =
    not (null wxs)
    && all isNormalizedAssoc (map snd wxs)
isNormalizedAssoc _ =
    True

-- | Check whether a 'Term' is a 'Var'.
isVar :: Term v -> Bool
isVar (Var _) = True
isVar _       = False

-- | Check whether a 'Term' is a 'Loc'.
isLoc :: Term v -> Bool
isLoc (Loc _) = True
isLoc _       = False

-- | Check whether a 'Term' is a 'Seq'.
isSeq :: Term v -> Bool
isSeq (Seq _) = True
isSeq _ = False

-- | Check whether a 'Term' is a 'Last'.
isLast :: Term v -> Bool
isLast (Last _) = True
isLast _ = False

-- | Check whether a 'Term' is a 'First'.
isFirst :: Term v -> Bool
isFirst (First _) = True
isFirst _ = False

-- | Check whether a 'Term' is a 'Choices'.
isChoices :: Term v -> Bool
isChoices (Choices _) = True
isChoices _ = False

-- | Check whether a 'Term' is a parallel operation,
-- i.e. 'Last' or 'First'.
isParallel :: Term v -> Bool
isParallel (First ts) = not (null ts)
isParallel (Last  ts) = not (null ts)
isParallel _          = False

-- | Maximal number of outcomes that run in parallel.
--
-- * The arguments of 'First' and 'Last' run in parallel.
-- * The arguments of 'Seq' and 'Choices do _not_ run in parallel.
maxParallel :: Term v -> Int
maxParallel (Seq   ts)    = maximum $ map maxParallel ts
maxParallel (Last  ts)    = sum $ map maxParallel ts
maxParallel (First ts)    = sum $ map maxParallel ts
maxParallel (Choices wts) = maximum $ map (maxParallel . snd) wts
maxParallel _             = 1

-- | Normalize a term to \"associative normal form\".
normalizeAssoc :: Term v -> Term v
normalizeAssoc = everywhere normalize1Assoc

-- | Apply a transformation everywhere; bottom-up.
--
-- See also [Scrap your boilerplate
-- ](https://www.microsoft.com/en-us/research/wp-content/uploads/2003/01/hmap.pdf)
-- .
everywhere :: (Term v -> Term v) -> Term v -> Term v
everywhere f = every
  where
    every = f . recurse

    recurse a@(Var _) = a
    recurse a@Never = a
    recurse a@Wait0 = a
    recurse a@(Wait _) = a
    recurse a@(Loc  _) = a
    recurse (Seq   xs) = Seq $ map every xs
    recurse (Last  xs) = Last $ map every xs
    recurse (First xs) = First $ map every xs
    recurse (Choices wxs) = Choices $ [ (w, every x) | (w, x) <- wxs]

-- | Summarize all nodes; top-down, left-to-right.
--
-- See also [Scrap your boilerplate
-- ](https://www.microsoft.com/en-us/research/wp-content/uploads/2003/01/hmap.pdf)
-- .
everything :: (r -> r -> r) -> (Term v -> r) -> (Term v -> r)
everything combine f = recurse
  where
    recurse x@(Var _)  = f x
    recurse x@Never    = f x
    recurse x@Wait0    = f x
    recurse x@(Wait _) = f x
    recurse x@(Loc  _) = f x
    recurse x@(Seq   xs) =
        foldl' combine (f x) $ map recurse xs
    recurse x@(Last  xs) =
        foldl' combine (f x) $ map recurse xs
    recurse x@(First xs) =
        foldl' combine (f x) $ map recurse xs
    recurse x@(Choices wxs) =
        foldl' combine (f x) $ [ recurse y | (_, y) <- wxs]

-- | Normalize a term to \"associative normal form\"
-- under the assumptions
-- that the arguments to the outermost constructor are already normalized.
normalize1Assoc :: Term v -> Term v
normalize1Assoc = id
    . normalizeEmpty
    . normalize1Assoc'

-- | Ensure that the lists in 'Seq', 'Last', 'First'
-- do not contain a term with the same constructor.
normalizeEmpty :: Term v -> Term v
normalizeEmpty (Seq   []) = Wait0
normalizeEmpty (Last  []) = Wait0
normalizeEmpty (First []) = Wait0
normalizeEmpty (Choices []) = Wait0
normalizeEmpty x = x

-- | Ensure that the lists in 'Seq', 'Last', 'First'
-- do not contain a term with the same constructor.
normalize1Assoc' :: Term v -> Term v
normalize1Assoc' (Seq xs) = Seq (concatMap f xs)
  where
    f Wait0 = []
    f (Seq ys) = ys
    f o = [o]
normalize1Assoc' (Last xs) = Last (concatMap f xs)
  where
    f Wait0 = []
    f (Last ys) = ys
    f o = [o]
normalize1Assoc' (First xs) = First (concatMap f xs)
  where
    f Wait0 = []
    f (First ys) = ys
    f o = [o]
normalize1Assoc' o = o
