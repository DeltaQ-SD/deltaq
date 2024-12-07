{-|
Copyright   : Predictable Network Solutions Ltd., 2024
License     : BSD-3-Clause
Maintainer  : neil.davies@pnsol.com
Description : Example plots
-}
module DeltaQ.Example.Plot where

import qualified Diagrams.Core as D
import qualified Diagrams.Backend.SVG as D
import qualified Graphics.Rendering.Chart as Chart
import qualified Graphics.Rendering.Chart.Backend.Diagrams as Chart

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
