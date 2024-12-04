module PlottingSupport
 ( asMaybe
 , toXY
 , toXY'
 , plotCDF
 , plotCDFs
 , plotCDFWithCentiles
 , plotInverseCDF
 , plotInverseCDFs
 , plotInverseCDFWithCentiles
 , module Graphics.Rendering.Chart.Easy
 )
where

import Graphics.Rendering.Chart.Easy
import DeltaQ

asMaybe :: Eventually a -> Maybe a
asMaybe (Occurs x) = Just x
asMaybe Abandoned  = Nothing

toXY :: (DeltaQ o, Enum (Duration o), Fractional (Duration o))
     => o -> [(Duration o, Probability o)]
toXY = toXY' 2048 0.05

-- | The number of points to plot, the 'overshoot' (as a fraction of the range), the outcome
toXY' :: (DeltaQ o, Enum (Duration o), Fractional (Duration o))
      => Int -> Double -> o -> [(Duration o, Probability o)]
toXY' numPoints overshoot o = dedup $ leftEdge ++ middle ++ rightEdge
  where
    range = upbX - lwbX
    eps = range / fromIntegral numPoints
    Occurs lwbX = earliest o
    Occurs upbX = deadline o
    probMass = 1 - failure o
    sw = successWithin o
    leftEdge
      = [(0,0), (lwbX - eps, 0), (lwbX, sw lwbX)]
    rightEdge
      = [(upbX, probMass), (upbX + (fromRational . toRational $ overshoot) * range, probMass)]
    middle
      | eps <= 0
        = []
      | otherwise
        = [(x, sw x) |  x <- enumFromThenTo (lwbX + eps) (lwbX + eps + eps) (upbX - eps)]
    dedup [] = []
    dedup (x:xs) = x : dedup' x xs
    dedup' x [] = []
    dedup' x (x':xs)
      | x == x'   = dedup' x xs
      | otherwise = x' : dedup' x' xs

-- | Plot CDF of an outcome - with a title
plotCDF :: (DeltaQ o, Enum (Duration o), Fractional (Duration o), Real (Duration o), Real (Probability o))
        => String -> o -> Layout Double Double
plotCDF title o = execEC $ do
  layout_title .=  title
  layout_x_axis . laxis_title .= "Time (s)"
  layout_x_axis . laxis_generate .= maybe autoAxis (\u' -> scaledAxis def (0, 1.05 * u')) (asMaybe $ cv1 <$> deadline o)
  layout_y_axis . laxis_title .= "Probabilty Mass"
  plot $ line "" [[(cv1 a, cv2  b) | (a,b) <- toXY o]]
  where
   cv1 = fromRational . toRational
   cv2 = fromRational . toRational

-- | Plot multiple CDFs in a single plot
plotCDFs :: (DeltaQ o, Enum (Duration o), Fractional (Duration o), Real (Duration o), Real (Probability o))
        => String -> [(String, o)] -> Layout Double Double
plotCDFs title namedOutcomes = execEC $ do
  layout_title .=  title
  layout_x_axis . laxis_title .= "Time (s)"
  layout_x_axis . laxis_generate .= maybe autoAxis (\u' -> scaledAxis def (0, 1.05 * u' )) maxX
  layout_y_axis . laxis_title .= "Probabilty Mass"
  mapM_ plotOne namedOutcomes
  where
   cv1 = fromRational . toRational
   cv2 = fromRational . toRational
   maxX =  fmap cv1 $ maximum $ map (asMaybe . deadline . snd) namedOutcomes
   plotOne (t, o) = plot $ line t [[(cv1 a, cv2 b) | (a,b) <- toXY o]]

-- | Annotate a CDF with Centiles
plotCDFWithCentiles :: (DeltaQ o, Enum (Duration o), Fractional (Duration o), Real (Duration o), Real (Probability o))
        => String -> [Probability o] -> o -> Layout Double Double
plotCDFWithCentiles title centiles o = execEC $ do
  layout_title .=  title
  layout_x_axis . laxis_title .= "Time (s)"
  layout_x_axis . laxis_generate .= maybe autoAxis (\u' -> scaledAxis def (0, 1.05 * u')) (asMaybe $ cv1 <$> deadline o)
  layout_y_axis . laxis_title .= "Probabilty Mass"
  plot $ line "" [[(cv1 a, cv2  b) | (a,b) <- toXY o]]
  mapM_ plotCentile centiles
  where
   cv1 = fromRational . toRational
   cv2 = fromRational . toRational
   plotCentile x = case quantile x o of
     Abandoned -> return ()
     Occurs y  -> plot $ liftEC $ do
       plot_lines_style . line_color .= opaque black
       plot_lines_style . line_dashes .= [5,5]
       plot_lines_limit_values .=
         [ [(LMin, LValue $ cv2 x),(LValue $ cv1 y, LValue $ cv2 x)]
         , [(LValue $ cv1 y, LValue $ cv2 x), (LValue $ cv1 y, LMin)]
         ]

-- | plot the inverse CDF, this allows for the tail of the distribution to better visualised
plotInverseCDF :: (DeltaQ o, Enum (Duration o), Fractional (Duration o), Real (Duration o), Real (Probability o))
               => String -> o -> Layout Double LogValue
plotInverseCDF title o = execEC $ do
  layout_title .=  title
  layout_x_axis . laxis_title .= "Time (s)"
  layout_x_axis . laxis_generate .= maybe autoAxis (\u' -> scaledAxis def (0, 1.05 * u')) (asMaybe $ cv1 <$> deadline o)
  layout_y_axis . laxis_title .= "Log Inverse Probabilty Mass"
  plot $ line "" [[(cv1 a, 1 - cv2  b) | (a,b) <- toXY o]]
  where
   cv1 = fromRational . toRational
   cv2 = fromRational . toRational

plotInverseCDFs :: (DeltaQ o, Enum (Duration o), Fractional (Duration o), Real (Duration o), Real (Probability o))
               => String -> [(String, o)] -> Layout Double LogValue
plotInverseCDFs title namedOutcomes = execEC $ do
  layout_title .=  title
  layout_x_axis . laxis_title .= "Time (s)"
  layout_x_axis . laxis_generate .= maybe autoAxis (\u' -> scaledAxis def (0, 1.05 * u')) maxX
  layout_y_axis . laxis_title .= "Log Inverse Probabilty Mass"
  mapM_ plotOne namedOutcomes
  where
   cv1 = fromRational . toRational
   cv2 = fromRational . toRational
   maxX =  fmap cv1 $ maximum $ map (asMaybe . deadline . snd) namedOutcomes
   plotOne (t, o) = plot $ line t [[(cv1 a, 1 - cv2 b) | (a,b) <- toXY o]]

plotInverseCDFWithCentiles :: (DeltaQ o, Enum (Duration o), Fractional (Duration o), Real (Duration o), Real (Probability o))
        => String -> [Probability o] -> o -> Layout Double LogValue
plotInverseCDFWithCentiles title centiles o = execEC $ do
  layout_title .=  title
  layout_x_axis . laxis_title .= "Time (s)"
  layout_x_axis . laxis_generate .= maybe autoAxis (\u' -> scaledAxis def (0, 1.05 * u')) (asMaybe $ cv1 <$> deadline o)
  layout_y_axis . laxis_title .= "Log Inverse Probabilty Mass"
  plot $ line "" [[(cv1 a, 1 - cv2  b) | (a,b) <- toXY o]]
  mapM_ plotCentile centiles
  where
   cv1 = fromRational . toRational
   cv2 = fromRational . toRational
   plotCentile y = case quantile y o of
     Abandoned -> return ()
     Occurs x  -> plot $ liftEC $ do
       plot_lines_style . line_color .= opaque black
       plot_lines_style . line_dashes .= [5,5]
       plot_lines_limit_values .=
         [ [(LMin, LValue $ cv2 $ 1 - y),(LValue $ cv1 x, LValue $ cv2 $ 1 - y)]
         , [(LValue $ cv1 x, LValue $ cv2 $ 1 - y), (LValue $ cv1 x, LMin)]
         ]
