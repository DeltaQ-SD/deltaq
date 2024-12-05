{-# LANGUAGE ScopedTypeVariables #-}

module DeltaQ.ClassSpec
    ( spec
    ) where

import Prelude

import DeltaQ.Class
    ( eventuallyFromMaybe
    )
import Test.Hspec
    ( Spec
    , describe
    , it
    )
import Test.QuickCheck
    ( (===)
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
