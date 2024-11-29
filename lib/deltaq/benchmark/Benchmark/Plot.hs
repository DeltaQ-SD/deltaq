{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Main
Description : Benchmark analysis
Copyright   : Predictable Network Solutions Ltd., 2024
License     : BSD-3-Clause
Maintainer  : neil.davies@pnsol.com
-}
module Benchmark.Plot where

import Data.String
    ( fromString
    )

import qualified Data.ByteString.Lazy as BL
import qualified Data.Csv as C
import qualified Data.Vector as V
import qualified Graphics.Vega.VegaLite as G

{-----------------------------------------------------------------------------
    Data
------------------------------------------------------------------------------}
type Time = Double -- in seconds

data Measurement = Measurement
    { mName :: String
        -- ^ Name used for grouping the expression
    , mTime :: Time
        -- ^ Time required to evaluate the expression to Normal Form.
    , mExpressionSize :: Int
        -- ^ Size of the expression as we write it down.
    , mValueComplexity :: Int
        -- ^ Complexity of the value represented by the expression.
    }
    deriving (Eq, Show, Read)

{-----------------------------------------------------------------------------
    Read
------------------------------------------------------------------------------}
readCsv :: FilePath -> String -> IO [Measurement]
readCsv fpath op = do
    file <- BL.readFile fpath
    let Right (_, measurements) = C.decodeByName file
    pure $ filter ((op ==) . mName) $ V.toList measurements

instance C.FromNamedRecord Measurement where
    parseNamedRecord r =
        mkMeasurement <$> r C..: "Name" <*> r C..: "Mean"
      where
        mkMeasurement name time =
            Measurement
                { mName = take 4 name
                , mTime = time
                , mExpressionSize = size
                , mValueComplexity = size -- FIXME: Record complexities.
                }
          where
            size = read $ drop (length prefix) $ name
            prefix = ".>>./m = " :: String

{-----------------------------------------------------------------------------
    Plot
------------------------------------------------------------------------------}
plotToHtmlFile :: FilePath -> [Measurement] -> IO ()
plotToHtmlFile fpath measurements =
    G.toHtmlFile fpath . G.toVegaLite $
        [ enc []
        , G.title ("Operation " <> fromString name) []
        , G.layer [ values, points ]
        , G.height 300
        , G.width 400
        ]
  where
    enc = G.encoding
        . G.position G.X
            [ G.PName "M"
            , G.PmType G.Quantitative
            , G.PAxis [ G.AxTickMinStep 1 ]
            ]
        . G.position G.Y [ G.PName "Time / ms", G.PmType G.Quantitative ]
    mkData xfs = G.dataFromColumns []
        . G.dataColumn "M" (G.Numbers xs)
        . G.dataColumn "Time / ms" (G.Numbers fs)
        $ []
      where (xs, fs) = unzip xfs

    name = mName $ head measurements
    xys = map (\m -> (fromIntegral $ mExpressionSize m, 1000 * mTime m)) measurements

    values = G.asSpec
        [ mkData xys
        , G.mark G.Line []
        ]
    points = G.asSpec
        [ mkData xys
        , G.mark G.Circle []
        ]
