{-|
Copyright   : Predictable Network Solutions Ltd., 2003-2024
License     : BSD-3-Clause
Description : Main benchmark of 'DQ'.
-}
module Main (main) where

import Benchmark.Plot
    ( Measurement (..)
    )
import Data.Traversable
    ( for
    )
import DeltaQ.Class
    ( DeltaQ (..)
    , Outcome (..)
    )
import DeltaQ.PiecewisePolynomial
    ( DQ
    , complexity
    )

import qualified Statistics.Types as Stat
import qualified Criterion as C
import qualified Criterion.Main as C
import qualified Criterion.Types as C
import qualified Options.Applicative as O

{-----------------------------------------------------------------------------
    Main
------------------------------------------------------------------------------}
-- | Command line arguments for the basic benchmark.
data BenchmarkArgs = BenchmarkArgs
    { resultFile :: FilePath
    }

benchmarkOptions :: O.Parser BenchmarkArgs
benchmarkOptions =
    BenchmarkArgs
    <$> O.strOption
        (  O.long "output"
        <> O.short 'o'
        <> O.metavar "FILENAME"
        <> O.help "File for benchmark results"
        )

commandLineOptions :: O.ParserInfo BenchmarkArgs
commandLineOptions =
    O.info (benchmarkOptions O.<**> O.helper)
      ( O.fullDesc
     <> O.progDesc "Run a benchmark of the `deltaq` package"
     <> O.header "basic - a benchmark for `deltaq`" )

main :: IO ()
main = do
    args <- O.execParser commandLineOptions
    measurements <- concat <$> sequence
        [ measureOp ".\\/." (.\/.) 20
        , measureOp "./\\." (./\.) 20
        , measureOp ".>>." (.>>.) 9
        ]
    writeFile (resultFile args) $ show measurements

{-----------------------------------------------------------------------------
    Benchmark
    Plumbing
------------------------------------------------------------------------------}
measureOp :: String -> (DQ -> DQ -> DQ) -> Int -> IO [Measurement]
measureOp name op mm = do
    for [0 .. mm] $ \m -> do
        let mkExpression = replicateOp op
        putStrLn $ "Measurement: " <> name <> ", m = " <> show m
        report <- C.benchmarkWith' config $ C.nf mkExpression m
        pure $ Measurement
            { mName = name
            , mTime = getTime report
            , mExpressionSize = m
            , mValueComplexity = complexity (mkExpression m)
            }
  where
    getTime = 
        Stat.estPoint . C.anMean . C.reportAnalysis 
    config = C.defaultConfig { C.timeLimit = 10 / fromIntegral mm }

{-----------------------------------------------------------------------------
    Benchmark
    Expressions
------------------------------------------------------------------------------}
-- | Construct an expression that applies a given binary operation
-- \( m \) times.
replicateOp :: (DQ -> DQ -> DQ) -> Int -> DQ
replicateOp _  0 = uniform 0 1
replicateOp op m =
    uniform x 1 `op` replicateOp op (m-1)
  where
    x = 1 - 1 / fromIntegral m
