{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

{-|
Copyright   : Neil Davies, 2024
              Predictable Network Solutions Ltd., 2024
License     : BSD-3-Clause
Maintainer  : neil.davies@pnsol.com
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
    , Eventually (..)
    , maybeFromEventually
    )
import Graphics.Rendering.Chart.Easy
    ( (.=)
    )

import qualified Graphics.Rendering.Chart.Easy as G

{-----------------------------------------------------------------------------
    Plot
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
plotCDF title o = G.execEC $ do
    G.layout_title .= title
    G.layout_x_axis . G.laxis_title .= "Time (s)"
    G.layout_x_axis
        . G.laxis_generate
        .= maybe
            G.autoAxis
            (\u' -> G.scaledAxis G.def (0, 1.05 * u'))
            (maybeFromEventually $ cv1 <$> deadline o)
    G.layout_y_axis . G.laxis_title .= "Cumulative Probabilty"
    G.plot $ G.line "" [[(cv1 a, cv2 b) | (a, b) <- toXY o]]
  where
    cv1 = fromRational . toRational
    cv2 = fromRational . toRational

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
    G.layout_x_axis . G.laxis_title .= "Time (s)"
    G.layout_x_axis
        . G.laxis_generate
        .= maybe G.autoAxis (\u' -> G.scaledAxis G.def (0, 1.05 * u')) maxX
    G.layout_y_axis . G.laxis_title .= "Cumulative Probabilty"
    mapM_ plotOne namedOutcomes
  where
    cv1 = fromRational . toRational
    cv2 = fromRational . toRational
    maxX =
        fmap cv1
        $ maximum
        $ map (maybeFromEventually . deadline . snd) namedOutcomes
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
    G.layout_x_axis . G.laxis_title .= "Time (s)"
    G.layout_x_axis
        . G.laxis_generate
        .= maybe
            G.autoAxis
            (\u' -> G.scaledAxis G.def (0, 1.05 * u'))
            (maybeFromEventually $ cv1 <$> deadline o)
    G.layout_y_axis . G.laxis_title .= "Cumulative Probabilty"
    G.plot $ G.line "" [[(cv1 a, cv2 b) | (a, b) <- toXY o]]
    mapM_ plotQuantile quantiles
  where
    cv1 = fromRational . toRational
    cv2 = fromRational . toRational
    plotQuantile x = case quantile x o of
        Abandoned -> pure ()
        Occurs y -> G.plot $ G.liftEC $ do
            G.plot_lines_style . G.line_color .= G.opaque G.black
            G.plot_lines_style . G.line_dashes .= [5, 5]
            G.plot_lines_limit_values
                .= [ [ (G.LMin, G.LValue $ cv2 x)
                     , (G.LValue $ cv1 y, G.LValue $ cv2 x)
                     ]
                   , [(G.LValue $ cv1 y, G.LValue $ cv2 x)
                     , (G.LValue $ cv1 y, G.LMin)
                     ]
                   ]

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
plotInverseCDF title o = G.execEC $ do
    G.layout_title .= title
    G.layout_x_axis . G.laxis_title .= "Time (s)"
    G.layout_x_axis
        . G.laxis_generate
        .= maybe
            G.autoAxis
            (\u' -> G.scaledAxis G.def (0, 1.05 * u'))
            (maybeFromEventually $ cv1 <$> deadline o)
    G.layout_y_axis . G.laxis_title .= "Log Inverse Cumulative Probabilty"
    G.plot $ G.line "" [[(cv1 a, 1 - cv2 b) | (a, b) <- toXY o]]
  where
    cv1 = fromRational . toRational
    cv2 = fromRational . toRational

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
    G.layout_x_axis . G.laxis_title .= "Time (s)"
    G.layout_x_axis
        . G.laxis_generate
        .= maybe G.autoAxis (\u' -> G.scaledAxis G.def (0, 1.05 * u')) maxX
    G.layout_y_axis . G.laxis_title .= "Log Inverse Cumulative Probabilty"
    mapM_ plotOne namedOutcomes
  where
    cv1 = fromRational . toRational
    cv2 = fromRational . toRational
    maxX =
        fmap cv1
        $ maximum
        $ map (maybeFromEventually . deadline . snd) namedOutcomes
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
    G.layout_x_axis . G.laxis_title .= "Time (s)"
    G.layout_x_axis
        . G.laxis_generate
        .= maybe
            G.autoAxis
            (\u' -> G.scaledAxis G.def (0, 1.05 * u'))
            (maybeFromEventually $ cv1 <$> deadline o)
    G.layout_y_axis . G.laxis_title .= "Log Inverse Cumulative Probabilty"
    G.plot $ G.line "" [[(cv1 a, 1 - cv2 b) | (a, b) <- toXY o]]
    mapM_ plotQuantile quantiles
  where
    cv1 = fromRational . toRational
    cv2 = fromRational . toRational
    plotQuantile y = case quantile y o of
        Abandoned -> pure ()
        Occurs x -> G.plot $ G.liftEC $ do
            G.plot_lines_style . G.line_color .= G.opaque G.black
            G.plot_lines_style . G.line_dashes .= [5, 5]
            G.plot_lines_limit_values
                .= [ [ (G.LMin, G.LValue $ cv2 $ 1 - y)
                     , (G.LValue $ cv1 x, G.LValue $ cv2 $ 1 - y)]
                   , [ (G.LValue $ cv1 x, G.LValue $ cv2 $ 1 - y)
                     , (G.LValue $ cv1 x, G.LMin)
                     ]
                   ]

{-----------------------------------------------------------------------------
    Helper functions
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
    dedup $ leftEdge <> middle <> rightEdge
  where
    range = upbX - lwbX
    eps = range / fromIntegral numPoints
    Occurs lwbX = earliest o
    Occurs upbX = deadline o
    success = 1 - failure o
    sw = successWithin o
    leftEdge =
        [(0, 0), (lwbX - eps, 0), (lwbX, sw lwbX)]
    rightEdge =
        [ (upbX, success)
        , (upbX + (fromRational . toRational $ overshoot) * range, success)
        ]
    middle
        | eps <= 0 = []
        | otherwise =
            [ (x, sw x)
            | x <- [lwbX + eps, lwbX + 2*eps .. upbX - eps]
            ]

-- | Remove neighboring occurrences of the same element from the list.
dedup :: Eq a => [a] -> [a]
dedup [] = []
dedup (x : xs) = x : dedup' x xs
  where
    dedup' _ [] = []
    dedup' y (y' : ys)
        | y == y' = dedup' y ys
        | otherwise = y' : dedup' y' ys
