{-# OPTIONS_GHC -Wno-unused-top-binds #-}

{-|
Copyright   : Predictable Network Solutions Ltd., 2024
License     : BSD-3-Clause
Maintainer  : neil.davies@pnsol.com
Description : Example plots
-}
module DeltaQ.Example.Plot
    ( -- * Plots
    -- $setup

    -- $plots
    ) where

import qualified Diagrams.Core as D
import qualified Diagrams.Backend.SVG as D
import qualified Diagrams.TwoD.Size as D
import qualified Graphics.Rendering.Chart as Chart
import qualified Graphics.Rendering.Chart.Backend.Diagrams as Chart

{- $setup
We need to import the following modules:

>>> import Prelude
>>> import DeltaQ
-}

{- $plots

Consider the following definitions:

>>> :{
e0, e1, e2 :: DQ
e0 = choice 0.0001 never $ uniform 0.5 1.5 .>>. uniform 0.5 3.5
e1 = choice 0.05 never $ e0 .>>. wait 0.5
e2 = e0 ./\. e1
:}

We plot them like this:

>>> plot_e0 <- toDiagram $ plotCDF "e0" e0

<<diagrams/e0.svg>>

>>> plot_e1 <- toDiagram $ plotCDF "e0" e0

<<diagrams/e1.svg>>

=== __(source code for generating images)__
>>> haddockRender "e0.svg" 500 plot_e0
File written:diagrams/e0.svg

>>> haddockRender "e1.svg" 500 plot_e1
File written:diagrams/e1.svg
-}

{-----------------------------------------------------------------------------
    Helper functions
    Plotting
------------------------------------------------------------------------------}
-- | Convert a 'Chart.Layout' into 'D.Diagram' for the 'D.SVG' backend.
--
-- Needs 'IO' in order to load font sizing information.
toDiagram
    :: Chart.PlotValue y
    => Chart.Layout Double y -> IO (D.Diagram D.SVG)
toDiagram chart = do
    denv <- newDEnv
    pure . fst $ Chart.runBackendR denv (Chart.toRenderable chart)

-- | Create a 'Chart.DEnv' with sensible defaults.
--
-- Needs 'IO' in order to load font sizing information.
newDEnv :: IO (Chart.DEnv Double)
newDEnv = Chart.defaultEnv Chart.vectorAlignmentFns 400 300

-- | Render a diagram for Haddock, for use with @doctest@.
haddockRender :: FilePath -> Int -> D.Diagram D.SVG -> IO ()
haddockRender filename w diagram = do
    let filepath = "diagrams/" <> filename
    D.renderSVG filepath (D.mkWidth $ fromIntegral w) diagram
    putStrLn $ "File written:" <> filepath
