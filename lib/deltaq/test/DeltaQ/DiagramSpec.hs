{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{-|
Copyright   : PLWORKZ R&D, 2025
License     : BSD-3-Clause
-}
module DeltaQ.DiagramSpec
    ( spec
    ) where

import Prelude

import Data.List
    ( sort
    )
import DeltaQ.Diagram
    ( Tile (..)
    , Token (..)
    , layout
    )
import DeltaQ.Expr
    ( O
    , Term (..)
    , everything
    , outcomeFromTerm
    )
import DeltaQ.Gen.Term
    ( genTerm
    )
import Test.Hspec
    ( Spec
    , describe
    , it
    )
import Test.QuickCheck
    ( Gen
    , Arbitrary
    , (===)
    , arbitrary
    , property
    , scale
    , vector
    )

{-----------------------------------------------------------------------------
    Tests
------------------------------------------------------------------------------}
spec :: Spec
spec = do
    describe "layout" $ do
        it "all 'Var' are rendered" $ property $
            \(o :: Term String) ->
                let fromVarTile (Tile _ _ (VarT name)) = [name]
                    fromVarTile _ = []

                    fromVarTerm (Var name) = [name]
                    fromVarTerm _ = []
                in
                    sort (everything (<>) fromVarTerm o)
                        === sort (concatMap fromVarTile $ layout o)

        it "all 'Loc' are rendered" $ property $
            \(o :: Term String) ->
                let fromLocTile (Tile _ _ (Close (Just s) _)) = [s]
                    fromLocTile (Tile _ _ (Location s)) = [s]
                    fromLocTile _ = []

                    fromLocTerm (Loc name) = [name]
                    fromLocTerm _ = []
                in
                    sort (everything (<>) fromLocTerm o)
                        === sort (concatMap fromLocTile $ layout o)

{-----------------------------------------------------------------------------
    Random generators
------------------------------------------------------------------------------}
instance Arbitrary (Term String) where
    arbitrary = scale (`div` 11) (genTerm genName)
      where genName = vector 2 :: Gen String

instance Arbitrary O where
    arbitrary = outcomeFromTerm <$> arbitrary
