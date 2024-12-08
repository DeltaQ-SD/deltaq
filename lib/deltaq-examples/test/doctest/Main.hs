{-|
Copyright   : Predictable Network Solutions Ltd., 2024
License     : BSD-3-Clause
Maintainer  : neil.davies@pnsol.com
Description : Run doctest on examples.
-}
module Main
    ( main
    ) where

import Test.DocTest

main :: IO ()
main = doctest
    [ "src"
    ]
