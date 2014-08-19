{-# LANGUAGE TemplateHaskell #-}              
module Tracker.Excitation ( Excitation
                          , excitePeriod, exciteAmp
                          , trajectory
                          , configureExcitation
                          , defaultExcitation
                          , phaseAmp
                          ) where

import Tracker.Types
import Tracker.LowLevel
import Tracker.Commands
import qualified Data.Vector as V
import Data.List (maximumBy)
import Data.Ord (comparing)
import Control.Applicative
import Control.Error
import Data.Traversable as T
import Control.Monad (void)
import Linear
import Control.Lens

import Data.Colour
import Data.Colour.Names as Colours
import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Backend.Cairo
import Data.Default

-- | Excitation amplitude
type Amplitude = Double

-- | Phase in number of samples
type Phase = Int

data Excitation t = Excitation { _excitePeriod :: t
                               , _exciteAmp    :: Amplitude
                               }
                deriving (Show, Eq, Read)
makeLenses ''Excitation     

trajectory :: Excitation Int -> V.Vector Double
trajectory exc = V.generate (exc ^. excitePeriod) (trajectory' exc')
  where exc' = excitePeriod %~ realToFrac $ exc           

trajectory' :: RealFrac a => Excitation a -> Int -> Double
trajectory' (Excitation period amp) i = 
    amp * sin (2*pi*realToFrac i/realToFrac period)

configureExcitation :: (Functor m, MonadIO m)
                    => Stage (Maybe (Excitation Int))
                    -> EitherT String (TrackerT m) ()
configureExcitation exc =
    void $ T.sequence $ go <$> stageAxes <*> exc
  where go axis (Just exc) = setExcitation axis
                             $ fmap round $ trajectory exc
        go axis Nothing    = setExcitation axis $ V.empty

defaultExcitation :: Stage (Excitation Int)
defaultExcitation = mkStage
    (Excitation 239 100)
    (Excitation 199 100)
    (Excitation 149 100)

correlate :: Num a => V.Vector a -> V.Vector a -> Phase -> a
correlate a b lag = (V.drop lag a V.++ a) `dot` b

mean :: Fractional a => V.Vector a -> a
mean v = V.sum v / realToFrac (V.length v)

phaseAmp :: (MonadIO m, Functor m)
         => Excitation Double -> V.Vector Double -> TrackerT m (Phase, Amplitude)
phaseAmp excitation samples = do
    let samples' = fmap (\x->x - mean samples) samples
        excLen = V.length exc
        exc = V.generate (ceiling $ excitation^.excitePeriod)
              $ trajectory' excitation
        sampleLen = 10 * excLen
        corr = [ (i, correlate (V.take sampleLen samples') exc i)
               | i <- [0..V.length exc]
               ]
        (phase, corrNorm) = maximumBy (comparing snd) corr
        amp = correlate (V.take excLen samples) exc phase / correlate exc exc 0
    plotSVG "corr.svg" $ layout_plots .~ [plotPoints corr] $ def
    plotSVG "phaseAmp.svg"
      $ layout_plots .~
        [ toPlot
            $ plot_points_values .~
                (V.toList $ V.indexed $ V.generate sampleLen
                $ \i->amp * trajectory' excitation i)
            $ plot_points_style . point_color .~ opaque Colours.blue $ def
        , toPlot
            $ plot_points_values .~
                (V.toList $ V.indexed $ V.take sampleLen $ V.drop phase samples')
            $ plot_points_style . point_color .~ opaque Colours.red $ def
        ] $ def
    let inPhase = V.drop phase samples'
    let corrected = V.imap (\i _->amp * trajectory' excitation i) inPhase
    plotSVG "r.svg" $ layout_plots .~ [plotPoints $ V.toList $ V.zip inPhase corrected] $ def
    return $ (phase, amp)
    
plotSVG :: (ToRenderable a, Functor m, MonadIO m) => FilePath -> a -> m ()
plotSVG fname a = void $ liftIO $ renderableToFile def (toRenderable a) fname

plotPoints :: (PlotValue x, PlotValue y) => [(x,y)] -> Plot x y
plotPoints pts = toPlot $ plot_points_values .~ pts $ def
