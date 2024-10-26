{-# LANGUAGE FlexibleContexts      #-}
module DeltaQ.Model.Utilities
  ( DeltaQVisualisation(..)
  , plotCDF
  , plotCDFs
  , plotCDFWithCentiles
  , plotInverseCDF
  , plotPDF
  , plotPDFs
  )
where

import DeltaQ.Model.DeltaQ

import Graphics.Rendering.Chart.Easy

import Data.Bifunctor (second)

class (DeltaQ irv) => DeltaQVisualisation irv where
  -- | Return a sequence of Time and Probability Mass points of a given length
  --   over the support of the ΔQ. The lower edge of a Heaviside is represented
  --   through the use of `Left`. The sequence is monotonitically increasing in
  --   both Time and Probablity Mass.
  asDiscreteCDF :: irv -> Int -> [Either (Time irv, ProbMass irv) [(Time irv, ProbMass irv)]]
  -- | Return a sequence of (Left) Impulse Probablity mass (equivalent to the
  --   integral of the Heaviside function at that point) or (Right) a sequence
  --   of Time and Probability Density. The sequence is monotonitcally increasing in Time.
  asDiscretePDF :: irv -> Int -> [Either (Time irv, ProbMass irv) [(Time irv, ProbMass irv)]]
  -- | Given a (monotonically increasing in both paramters) list of `Time`,
  --   `ProbMass` pairs, create a stepwise QTA.
  fromQTA :: [(Time irv , ProbMass irv)] -> irv
  -- | Given a (monotonically increasing in both paramters) list of `Time`,
  --   `ProbMass` pairs, create a linearaly interpolated improper random
  --   variable. It is assumed that the first point is at (0,0).
  fromEmpirical :: [(Time irv, ProbMass irv)] -> irv


plotCDF :: ( PlotValue (ProbMass irv), PlotValue (Time irv)
           , RealFloat (Time irv), Show (Time irv)
           , DeltaQVisualisation irv)
        => String
        -> irv
        -> Layout (Time irv) (ProbMass irv)
plotCDF title irv = execEC $ do
  layout_title .=  title
  layout_x_axis . laxis_title .= "Time"
  layout_x_axis . laxis_generate .= maybe autoAxis (\u' -> scaledAxis def (0, factor * u')) u
  layout_y_axis . laxis_title .= "Prob. Mass"
  plot $ line "" [cdf ++ maybe [] (\u' -> [(2 * factor * u', tangibleMass irv)]) u]
  where
   cdf = concatMap (either (:[]) id) $  asDiscreteCDF irv 1000
   factor = 1.1
   (_,u) = support irv

plotCDFs :: ( PlotValue (ProbMass irv), PlotValue (Time irv)
            , RealFloat (Time irv), Show (Time irv)
            , DeltaQVisualisation irv)
         => String
         -> [(String, irv)]
         -> Layout (Time irv) (ProbMass irv)

plotCDFs title irvs = execEC $ do
  layout_title .=  title
  layout_x_axis . laxis_title .= "Time"
  layout_x_axis . laxis_generate .= maybe autoAxis (\u' -> scaledAxis def (0, factor * u')) maxSupport
  layout_y_axis . laxis_title .= "Prob. Mass"
  mapM_ plotOne irvs
  where
   maxSupport = maximum $ map (snd . support . snd) irvs
   plotOne (t, irv)
     = plot $ line t [cdf irv 1000 ++ maybe [] (\u' -> [(2 * factor * u', tangibleMass irv)]) (snd $ support irv)]
   cdf a b = concatMap (either (:[]) id) $ asDiscreteCDF a b
   factor = 1.1

plotCDFWithCentiles :: ( PlotValue (ProbMass irv), PlotValue (Time irv)
                       , RealFloat (Time irv), Show (Time irv)
                       , DeltaQVisualisation irv)
                    => String
                    -> irv
                    -> [ProbMass irv]
                    -> Layout (Time irv) (ProbMass irv)

plotCDFWithCentiles title irv cs = execEC $ do
  layout_title .=  title
  layout_x_axis . laxis_title .= "Time"
  layout_x_axis . laxis_generate .= maybe autoAxis (\u' -> scaledAxis def (0, factor * u')) u
  layout_y_axis . laxis_title .= "Prob. Mass"
  plot $ line "" [cdf ++ maybe [] (\u' -> [(2 * factor * u', tangibleMass irv)]) u]
  mapM_ plotCentile cs
  where
   cdf = concatMap (either (:[]) id) $  asDiscreteCDF irv 1000
   factor = 1.1
   (_,u) = support irv
   plotCentile x = case centile irv x of
     Nothing -> return ()
     Just y  -> plot $ liftEC $ do
       plot_lines_style . line_color .= opaque black
       plot_lines_style . line_dashes .= [5,5]
       plot_lines_limit_values .=
         [ [(LMin, LValue x),(LValue y, LValue x)]
         , [(LValue y, LValue x), (LValue y, LMin)]
         ]

plotInverseCDF :: ( PlotValue (ProbMass irv), PlotValue (Time irv)
                  , RealFloat (Time irv), Show (Time irv)
                  , Real (ProbMass irv)
                  , DeltaQVisualisation irv)
               => String
               -> irv
               -> Layout (Time irv) LogValue
plotInverseCDF title irv = execEC $ do
  layout_title .=  title
  layout_x_axis . laxis_title .= "Time"
  layout_x_axis . laxis_generate .= maybe autoAxis (\u' -> scaledAxis def (0, factor * u')) u
  layout_y_axis . laxis_title .= "Log Inv. Prob. Mass"
  plot $ line "" [map (second cv) cdf
                  ++ maybe [] (\u' -> [(2 * factor * u', cv $ tangibleMass irv)]) u]
  where
   cdf = concatMap (either (:[]) id) $  asDiscreteCDF irv 1000
   factor = 1.1
   (_,u) = support irv
   cv = fromRational . toRational . complement

plotPDF :: ( PlotValue (ProbMass irv), PlotValue (Time irv)
           , RealFloat (Time irv), Show (Time irv)
           , DeltaQVisualisation irv)
        => String
        -> irv
        -> Layout (Time irv) (ProbMass irv)
plotPDF title irv = execEC $ do
  layout_title .=  title
  layout_x_axis . laxis_title .= "Time"
  layout_x_axis . laxis_generate .= maybe autoAxis (\u' -> scaledAxis def (0, factor * u')) u
  layout_y_axis . laxis_title .= "Prob. Density"
  plot $ line "" $ map (either (\(t,p) -> [(t,never), (t, p)]) id) $ asDiscretePDF irv 1000
  where
   factor = 1.1
   (_,u) = support irv

plotPDFs :: ( PlotValue (ProbMass irv), PlotValue (Time irv)
            , RealFloat (Time irv), Show (Time irv)
            , DeltaQVisualisation irv)
         => String
         -> [(String, irv)]
         -> Layout (Time irv) (ProbMass irv)
plotPDFs title irvs = execEC $ do
  layout_title .=  title
  layout_x_axis . laxis_title .= "Time"
  layout_x_axis . laxis_generate .= maybe autoAxis (\u' -> scaledAxis def (0, factor * u')) maxSupport
  layout_y_axis . laxis_title .= "Prob. Density"
  mapM_ plotOne irvs
  where
   maxSupport = maximum $ map (snd . support . snd) irvs
   factor = 1.1
   plotOne (t, irv)
     = plot $ line t $ map (either (\(t',p) -> [(t',never), (t', p)]) id) $ asDiscretePDF irv 1000
