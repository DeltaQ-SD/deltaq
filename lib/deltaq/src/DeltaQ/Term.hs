{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

{-|
Copyright   : Predictable Network Solutions Ltd., 2020-2024
License     : BSD-3-Clause
Description : Instances via term representation.

'O' is an outcome expression represented as a term,
using the numeric type 'Rational'.
-}
module DeltaQ.Term
    ( -- * Outcome expressions
      O
    , var
    , toOutcome
    , termFromOutcome
    , outcomeFromTerm

    -- * Outcome terms
    , Term (..)
    , isNormalized
    ) where

import Control.DeepSeq
    ( NFData
    )
import Data.List
    ( sort
    )
import DeltaQ.Class
    ( Outcome (..)
    )
import GHC.Generics
    ( Generic
    )

{-----------------------------------------------------------------------------
    O
------------------------------------------------------------------------------}
-- | Outcome expression, represented as a normalized term.
newtype O = O (Term String)
    -- INVARIANT: Terms are always normalized.
    deriving (Show, Generic, NFData)

-- | Two outcome expressions are equal if and only if they can be transformed
-- into each other using the properties required by the 'Outcome' class,
-- listed here: "DeltaQ.Class#p:Outcome".
--
-- Two 'var' are considered equal if and only if they have the same name.
deriving instance Eq O

-- | Outcome variable, referenced by a unique name.
var :: String -> O
var = O . Var

-- | Abstract outcome expressions can be mapped to more concrete types,
-- such as 'DeltaQ.PiecewisePolynomial.DQ',
-- provided that we know how to map 'var'.
toOutcome
    ::  ( Outcome o
        , Duration o ~ Rational
        ) 
    => (String -> o) -> O -> o
toOutcome f (O term) = go term
  where
    go (Var v) = f v
    go Never = never
    go (Wait t) = wait t
    go (Seq xs) = foldr1 (.>>.) $ map go xs
    go (Last xs) = foldr1 (./\.) $ map go xs
    go (First xs) = foldr1 (.\/.) $ map go xs

-- | Outcome expressions are instances of 'Outcome'.
instance Outcome O where
    type Duration O = Rational

    never = O Never
    wait = O . Wait
    sequentially (O x) (O y) = O . normalize1 $ Seq [x,y]
    firstToFinish (O x) (O y) = O . normalize1 $ First [x,y]
    lastToFinish (O x) (O y) = O . normalize1 $ Last [x,y]

-- | Convert an outcome expression to its normalized 'Term' representation.
-- 
-- > x == y  implies   termFromOutcome x == termFromOutcome y
-- >
-- > isNormalized (termFromOutcome x)  =  True
termFromOutcome :: O -> Term String
termFromOutcome (O term) = term

-- | Construct an outcome expression 'O' from an outcome 'Term'.
--
-- Internally, the outcome expression will be represented
-- as the normalization of the given term.
outcomeFromTerm :: Term String -> O
outcomeFromTerm = O . normalize

{-----------------------------------------------------------------------------
    Terms
------------------------------------------------------------------------------}
-- | Term representation for outcomes.
--
-- Different terms may represent equal outcomes expressions.
-- 
data Term v
    = Var v
    | Never
    | Wait Rational
    | Seq [Term v]
    | Last [Term v]
    | First [Term v]
    deriving (Show, Eq, Ord, Generic, NFData)

-- | Predicate that defines what it means for a 'Term' to be normalized.
--
-- Each 'Term' has a unique normalization.
isNormalized :: Ord v => Term v -> Bool
isNormalized (Seq xs) =
    not (null xs)
    && all (not . isNever) xs
    && all (not . isSeq) xs
    && and (pairsWith isNotBothWait xs)
    && and (pairsWith (<=) xs)
    && all isNormalized xs
isNormalized (Last xs) =
    not (null xs)
    && all (not . isNever) xs
    && all (not . isLast) xs
    && and (pairsWith isNotBothWait xs)
    && and (pairsWith (<=) xs)
    && all isNormalized xs
isNormalized (First xs) =
    not (null xs)
    && all (not . isNever) xs
    && all (not . isFirst) xs
    && and (pairsWith isNotBothWait xs)
    && and (pairsWith (<=) xs)
    && all isNormalized xs
isNormalized _ =
    True

isNever :: Term v -> Bool
isNever Never = True
isNever _ = False

isSeq :: Term v -> Bool
isSeq (Seq _) = True
isSeq _ = False

isLast :: Term v -> Bool
isLast (Last _) = True
isLast _ = False

isFirst :: Term v -> Bool
isFirst (First _) = True
isFirst _ = False

isNotBothWait :: Term v -> Term v -> Bool
isNotBothWait (Wait _) (Wait _) = False
isNotBothWait _ _ = True

pairsWith :: (a -> a -> b) -> [a] -> [b]
pairsWith f xs = zipWith f xs (drop 1 xs)

-- | Normalize a term.
--
-- 'normalize' will give equal results
-- if and only if two terms are equal according to the properties.
--
-- The strategy for normalization is as follows:
--
-- * Associativity of '(.>>.)', '(./\.)', and '(.\/.)' is
--   enforced by the use of lists in the definition 'Seq', 'Last', 'First'.
--
-- * The following properties are handled by 'normalize1'
--
--   * Absorption of 'never'
--   * Combination of 'wait'
--   * Commutativitiy of '(./\.)', and '(.\/.)'
--
normalize :: Ord v => Term v -> Term v
normalize = everywhere normalize1

-- | Apply a transformation everywhere; bottom-up.
--
-- See also [Scrap your boilerplate
-- ](https://www.microsoft.com/en-us/research/wp-content/uploads/2003/01/hmap.pdf)
everywhere :: (Term v -> Term v) -> Term v -> Term v
everywhere f = every
  where
    every = f . recurse

    recurse a@(Var _) = a
    recurse a@Never = a
    recurse a@(Wait _) = a
    recurse (Seq xs) = Seq $ map every xs
    recurse (Last xs) = Last $ map every xs
    recurse (First xs) = First $ map every xs

-- | Normalize a term under the assumption
-- that the arguments to the outermost constructor are already normalized.
normalize1 :: Ord v => Term v -> Term v
normalize1 = id
    . normalize1Sym
    . normalize1Wait
    . normalize1Never
    . normalize1Assoc

-- | Ensure that the lists in 'Seq', 'Last', 'First'
-- do not contain a term with the same constructor.
normalize1Assoc :: Ord v => Term v -> Term v
normalize1Assoc (Seq xs) = Seq (concatMap f xs)
  where
    f (Seq ys) = ys
    f o = [o]
normalize1Assoc (Last xs) = Last (concatMap f xs)
  where
    f (Last ys) = ys
    f o = [o]
normalize1Assoc (First xs) = First (concatMap f xs)
  where
    f (First ys) = ys
    f o = [o]
normalize1Assoc o = o

-- | Absorb 'Never'.
normalize1Never :: Term v -> Term v
normalize1Never (Seq xs)
    | any isNever xs = Never
    | otherwise = Seq xs
normalize1Never (Last xs)
    | any isNever xs = Never
    | otherwise = Last xs
normalize1Never (First xs) =
    First $ filter (not . isNever) xs
normalize1Never o = o

-- | Combine adjacent 'Wait'.
normalize1Wait :: Term v -> Term v
normalize1Wait (Seq xs) = Seq (combinePairs f xs)
  where
    f (Wait t) (Wait s) = Just $ Wait (t + s)
    f _ _ = Nothing
normalize1Wait (Last xs) = Last (combinePairs f xs)
  where
    f (Wait t) (Wait s) = Just $ Wait (max t s)
    f _ _ = Nothing
normalize1Wait (First xs) = First (combinePairs f xs)
  where
    f (Wait t) (Wait s) = Just $ Wait (min t s)
    f _ _ = Nothing
normalize1Wait o = o

combinePairs :: (a -> a -> Maybe a) -> [a] -> [a]
combinePairs f (x:y:xs)
    | Just z <- f x y = combinePairs f (z:xs)
    | otherwise = x : combinePairs f (y:xs)
combinePairs _ xs = xs

-- | Ensure that the lists in 'Last', 'First' are sorted.
normalize1Sym :: Ord v => Term v -> Term v
normalize1Sym (Last xs) = Last (sort xs)
normalize1Sym (First xs) = First (sort xs)
normalize1Sym o = o
