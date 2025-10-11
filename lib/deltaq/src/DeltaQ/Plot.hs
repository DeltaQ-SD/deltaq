{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

{-|
Copyright   : Predictable Network Solutions Ltd., 2003-2024
License     : BSD-3-Clause
Description : Plot 'DeltaQ'.

Plot instances of 'DeltaQ' using "Graphics.Rendering.Chart".
-}
module DeltaQ.Plot
    ( plotCDF
    , plotCDFs
    , plotCDFWithQuantiles
    , plotInverseCDF
    , plotInverseCDFs
    , plotInverseCDFWithQuantiles
    ) where

import DeltaQ.Class
    ( Outcome (Duration)
    , DeltaQ (..)
    , ProbabilisticOutcome (Probability)
    , Eventually (..)
    , eventually
    , maybeFromEventually
    )
import Graphics.Rendering.Chart.Easy
    ( (.=)
    )

import qualified Graphics.Rendering.Chart.Easy as G

{-----------------------------------------------------------------------------
    Plot
    CDF
------------------------------------------------------------------------------}
-- | Plot the cumulative distribution function (CDF) of a 'DeltaQ',
-- with title.
plotCDF
    :: ( DeltaQ o
       , Enum (Duration o)
       , Fractional (Duration o)
       , Real (Duration o)
       , Real (Probability o)
       )
    => String -- ^ Title
    -> o -- ^ Outcome to plot
    -> G.Layout Double Double
plotCDF title o =
    plotCDFs title [("", o)]

-- | Plot multiple CDFs in a single plot,
-- with title.
plotCDFs
    :: ( DeltaQ o
       , Enum (Duration o)
       , Fractional (Duration o)
       , Real (Duration o)
       , Real (Probability o)
       )
    => String -- ^ Title
    -> [(String, o)] -- ^ Outcomes with names
    -> G.Layout Double Double
plotCDFs title namedOutcomes = G.execEC $ do
    G.layout_title .= title
    add_x_axis (map snd namedOutcomes)
    G.layout_y_axis . G.laxis_title .= "Cumulative Probabilty"
    mapM_ plotOne namedOutcomes
  where
    cv1 = fromRational . toRational
    cv2 = fromRational . toRational
    plotOne (t, o) = G.plot $ G.line t [[(cv1 a, cv2 b) | (a, b) <- toXY o]]

-- | Plot the cumulative distribution function (CDF) of a 'DeltaQ',
-- with title, and annotated with quantiles.
plotCDFWithQuantiles
    :: ( DeltaQ o
       , Enum (Duration o)
       , Fractional (Duration o)
       , Real (Duration o)
       , Real (Probability o)
       )
    => String -- ^ Title
    -> [Probability o] -- ^ Quantiles to highlight
    -> o -- ^ Outcome to plot
    -> G.Layout Double Double
plotCDFWithQuantiles title quantiles o = G.execEC $ do
    G.layout_title .= title
    add_x_axis [o]
    G.layout_y_axis . G.laxis_title .= "Cumulative Probabilty"
    G.plot $ G.line "" [[(cv1 a, cv2 b) | (a, b) <- toXY o]]
    mapM_ plotQuantile quantiles
  where
    cv1 = fromRational . toRational
    cv2 = fromRational . toRational
    plotQuantile y = case quantile o y of
        Abandoned -> pure ()
        Occurs x -> G.plot $ pure $ focusOnPoint (cv1 x, cv2 y)

{-----------------------------------------------------------------------------
    Plot
    Inverse CDF
------------------------------------------------------------------------------}
-- | Plot the inverse cumulative distribution function (CDF) of a 'DeltaQ',
-- with title.
--
-- Visualizes the tail of the distribution better.
plotInverseCDF
    :: ( DeltaQ o
       , Enum (Duration o)
       , Fractional (Duration o)
       , Real (Duration o)
       , Real (Probability o)
       )
    => String -- ^ Title
    -> o -- ^ Outcome
    -> G.Layout Double G.LogValue
plotInverseCDF title o =
    plotInverseCDFs title [("", o)]

-- | Plot the mulltiple inverse CDFs of a 'DeltaQ',
-- with title.
--
-- Visualizes the tail of the distribution better.
plotInverseCDFs
    :: ( DeltaQ o
       , Enum (Duration o)
       , Fractional (Duration o)
       , Real (Duration o)
       , Real (Probability o)
       )
    => String -- ^ Title
    -> [(String, o)] -- Outcomes with names
    -> G.Layout Double G.LogValue
plotInverseCDFs title namedOutcomes = G.execEC $ do
    G.layout_title .= title
    add_x_axis (map snd namedOutcomes)
    G.layout_y_axis . G.laxis_title .= "Log Inverse Cumulative Probabilty"
    mapM_ plotOne namedOutcomes
  where
    cv1 = fromRational . toRational
    cv2 = fromRational . toRational
    plotOne (t, o) = G.plot $ G.line t [[(cv1 a, 1 - cv2 b) | (a, b) <- toXY o]]

-- | Plot the cumulative distribution function (CDF) of a 'DeltaQ',
-- with title, and annotated with quantiles.
--
-- Visualizes the tail of the distribution better.
plotInverseCDFWithQuantiles
    :: ( DeltaQ o
       , Enum (Duration o)
       , Fractional (Duration o)
       , Real (Duration o)
       , Real (Probability o)
       )
    => String -- ^ Title
    -> [Probability o] -- ^ Quantiles to highlight
    -> o -- ^ Outcome to plot
    -> G.Layout Double G.LogValue
plotInverseCDFWithQuantiles title quantiles o = G.execEC $ do
    G.layout_title .= title
    add_x_axis [o]
    G.layout_y_axis . G.laxis_title .= "Log Inverse Cumulative Probabilty"
    G.plot $ G.line "" [[(cv1 a, 1 - cv2 b) | (a, b) <- toXY o]]
    mapM_ plotQuantile quantiles
  where
    cv1 = fromRational . toRational
    cv2 = fromRational . toRational
    plotQuantile y = case quantile o y of
        Abandoned -> pure ()
        Occurs x -> G.plot $ pure $ focusOnPoint (cv1 x, cv2 (1 - y))

{-----------------------------------------------------------------------------
    Helper functions
    Plot
------------------------------------------------------------------------------}
-- | Add a common @x@-axis to the plot.
add_x_axis 
    :: (DeltaQ o, Real (Duration o), Fractional (Duration o), G.PlotValue y)
    => [o]
    -> G.EC (G.Layout Double y) ()
add_x_axis outcomes = do
    G.layout_x_axis . G.laxis_title .= "Time (s)"
    G.layout_x_axis
        . G.laxis_generate
        .= maybe G.autoAxis (\u' -> G.scaledAxis G.def (0, 1.05 * u')) maxX
  where
    fromDuration = fromRational . toRational
    maxX = case outcomes of
        [] -> Nothing
        _  -> 
            fmap fromDuration
            $ maximum
            $ map (maybeFromEventually . deadline) outcomes

-- | Focus on a point by plotting dashed lines that connect it to the axes.
focusOnPoint
    :: (G.PlotValue x, G.PlotValue y)
    => (x,y) -> G.PlotLines x y
focusOnPoint (x,y) = G.execEC $ do
    G.plot_lines_style . G.line_color .= G.opaque G.black
    G.plot_lines_style . G.line_dashes .= [5, 5]
    G.plot_lines_limit_values .=
        [ [(G.LMin, G.LValue y), (G.LValue x, G.LValue y)]
        , [(G.LValue x, G.LValue y), (G.LValue x, G.LMin)]
        ]

{-----------------------------------------------------------------------------
    Helper functions
    Calculations
------------------------------------------------------------------------------}
-- | Create a graph for an 'Outcome', with sensible defaults for plotting.
toXY
    :: (DeltaQ o, Enum (Duration o), Fractional (Duration o))
    => o
    -> [(Duration o, Probability o)]
toXY = toXY' 2048 0.05

-- | Create a graph for an 'Outcome', given some parameters.
toXY'
    :: (DeltaQ o, Enum (Duration o), Fractional (Duration o))
    => Int -- ^ Number of points to plot.
    -> Double -- ^ \"Overshoot\" (as a fraction of the range)
    -> o -- ^ Outcome to convert
    -> [(Duration o, Probability o)]
toXY' numPoints overshoot o =
    deduplicate $ leftEdge <> middle <> rightEdge
  where
    range = upperX - lowerX
    eps = range / fromIntegral numPoints
    lowerX = eventually 0 id $ earliest o
    upperX = eventually halfLifeCarbon14 id $ deadline o
    halfLifeCarbon14 = 5730 * 365 * 24 * 60 * 60
    success = 1 - failure o
    sw = successWithin o
    leftEdge =
        [(0, 0), (lowerX - eps, 0), (lowerX, sw lowerX)]
    rightEdge =
        [ (upperX, success)
        , (upperX + (fromRational . toRational $ overshoot) * range, success)
        ]
    middle
        | eps <= 0 = []
        | otherwise =
            [ (x, sw x)
            | x <- [lowerX + eps, lowerX + 2*eps .. upperX - eps]
            ]

-- | Remove neighboring occurrences of the same element from the list.
deduplicate :: Eq a => [a] -> [a]
deduplicate [] = []
deduplicate (x : xs) = x : dedup' x xs
  where
    dedup' _ [] = []
    dedup' y (y' : ys)
        | y == y' = dedup' y ys
        | otherwise = y' : dedup' y' ys
