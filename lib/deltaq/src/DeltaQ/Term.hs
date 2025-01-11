{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
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
    ( -- * Type
      O
    , atom
    , toOutcome
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
-- | Outcome expression, represented as a term.
newtype O = O (Term String)
    -- INVARIANT: Terms are always normalized.
    deriving (Eq, Show, Generic, NFData)

-- | Outcome, abstract, referenced by a unique name.
atom :: String -> O
atom = O . Atom

-- | Abstract outcome expressions can be mapped to more concrete types,
-- such as 'DeltaQ.PiecewisePolynomial.DQ',
-- provided that we know how to map 'atom'.
toOutcome
    ::  ( Outcome o
        , Duration o ~ Rational
        ) 
    => (String -> o) -> O -> o
toOutcome f (O term) = go term
  where
    go (Atom v) = f v
    go Never = never
    go (Wait t) = wait t
    go (Seq xs) = foldr1 (.>>.) $ map go xs
    go (Last xs) = foldr1 (./\.) $ map go xs
    go (First xs) = foldr1 (.\/.) $ map go xs

{-----------------------------------------------------------------------------
    Terms
------------------------------------------------------------------------------}
-- | Term representation for outcomes.
--
-- Associativity of '(.>>.)', '(./\.)', and '(.\/.)' is baked into lists.
--
-- Normalization ensures that the required properties hold:
--
-- * Absorption of 'never'
-- * Combination of 'wait'
-- * Commutativitiy of '(.>>.)', '(./\.)', and '(.\/.)'
data Term v
    = Atom v
    | Never
    | Wait Rational
    | Seq [Term v]
    | Last [Term v]
    | First [Term v]
    deriving (Show, Eq, Ord, Generic, NFData)
