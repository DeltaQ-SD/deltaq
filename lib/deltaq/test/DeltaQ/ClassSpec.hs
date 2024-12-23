{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{-|
Copyright   : Predictable Network Solutions Ltd., 2003-2024
License     : BSD-3-Clause
-}
module DeltaQ.ClassSpec
    ( spec
    ) where

import Prelude

import DeltaQ.Class
    ( Eventually (..)
    , eventually
    , eventuallyFromMaybe
    , maybeFromEventually
    )
import Test.Hspec
    ( Spec
    , describe
    , it
    )
import Test.QuickCheck
    ( Arbitrary (..)
    , (===)
    , property
    )

{-----------------------------------------------------------------------------
    Tests
------------------------------------------------------------------------------}
spec :: Spec
spec = do
    describe "Eventually" $ do
        describe "eventuallyFromMaybe" $ do
            let morphism = eventuallyFromMaybe

            it "Eq" $ property $
                \(mx :: Maybe Integer) my ->
                    (morphism mx == morphism my)
                        === (mx == my)

            it "Functor" $ property $
                \(mx :: Maybe Integer) ->
                    let f = (2*)
                    in  fmap f (morphism mx)
                        === morphism (fmap f mx)

            it "Applicative, pure" $ property $
                \(x :: Integer) ->
                    pure x
                        === morphism (pure x)

            it "Applicative, (<*>)" $ property $
                \(mx :: Maybe Integer) my ->
                    let f = (+)
                    in  (f <$> morphism mx <*> morphism my)
                        === morphism (f <$> mx <*> my)

        it "maybeFromEventually . eventuallyFromMaybe" $ property $
            \(mx :: Maybe Bool) ->
                maybeFromEventually (eventuallyFromMaybe mx)
                    === mx

        it "eventually Abandoned Occurs = id" $ property $
            \(ex :: Eventually Bool) ->
                eventually Abandoned Occurs ex 
                    === ex

{-----------------------------------------------------------------------------
    Tests
------------------------------------------------------------------------------}
instance Arbitrary a => Arbitrary (Eventually a) where
    arbitrary = eventuallyFromMaybe <$> arbitrary
