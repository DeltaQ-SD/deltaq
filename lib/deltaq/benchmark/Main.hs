{-|
Module      : Main
Description : Main benchmark of 'DQ'.
Copyright   : Predictable Network Solutions Ltd., 2024
License     : BSD-3-Clause
Maintainer  : neil.davies@pnsol.com
-}
module Main (main) where

import Criterion.Main
    ( Benchmark
    , bench
    , bgroup
    , defaultMain
    , nf
    )
import DeltaQ.Class
    ( DeltaQ (..)
    , Outcome (..)
    )
import DeltaQ.PiecewisePolynomial
    ( DQ
    )

{-----------------------------------------------------------------------------
    Main
------------------------------------------------------------------------------}
main :: IO ()
main =
    defaultMain
        [ benchOp ".\\/." (.\/.) 10
        , benchOp "./\\." (./\.) 10
        , benchOp ".>>." (.>>.) 7
        ]

benchOp :: String -> (DQ -> DQ -> DQ) -> Int -> Benchmark
benchOp name op mm =
    bgroup name
        [ bench ("m = " <> show m) $ nf (replicateOp op) m
        | m <- [0 .. mm]
        ]

{-----------------------------------------------------------------------------
    Benchmark
------------------------------------------------------------------------------}
-- | Construct an expression that applies a given binary operation
-- \( m \) times.
replicateOp :: (DQ -> DQ -> DQ) -> Int -> DQ
replicateOp _  0 = uniform 0 1
replicateOp op m =
    uniform 0 1 `op` (wait t .>>. replicateOp op (m-1))
  where
    t = 1.5 * 1 / fromIntegral m
