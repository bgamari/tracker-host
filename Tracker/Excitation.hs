{-# LANGUAGE TemplateHaskell #-}              
module Tracker.Excitation ( Excitation
                          , excitePeriod, exciteAmp
                          , excitationTrajectory
                          , configureExcitation
                          , defaultExcitation
                          , phaseAmp
                          ) where

import Tracker.Types
import Tracker.LowLevel
import Tracker.Commands
import qualified Data.Vector as V
import Data.Int
import Data.List (maximumBy)
import Data.Ord (comparing)
import Control.Applicative
import Control.Error
import Data.Traversable as T
import Control.Monad (void)
import Linear
import Control.Lens

import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Backend.Cairo
import Data.Default

-- | Excitation amplitude
type Amplitude = Double

-- | Phase in number of samples
type Phase = Int

data Excitation = Excitation { _excitePeriod :: Int
                             , _exciteAmp    :: Amplitude
                             }
                deriving (Show, Eq, Read)
makeLenses ''Excitation     

excitationTrajectory :: Excitation -> V.Vector Double
excitationTrajectory (Excitation period amp) = V.generate period f
  where f i = amp * sin (2*pi*realToFrac i/realToFrac period)

configureExcitation :: (Functor m, MonadIO m)
                    => Stage (Maybe Excitation)
                    -> EitherT String (TrackerT m) ()
configureExcitation exc =
    void $ T.sequence $ go <$> stageAxes <*> exc
  where go axis (Just exc) = setExcitation axis
                             $ fmap round $ excitationTrajectory exc
        go axis Nothing    = setExcitation axis $ V.empty

defaultExcitation :: Stage Excitation
defaultExcitation = mkStage
    (Excitation 239 100)
    (Excitation 199 100)
    (Excitation 149 100)

correlate :: Num a => V.Vector a -> V.Vector a -> Phase -> a
correlate a b lag = (V.drop lag a V.++ a) `dot` b

mean :: Fractional a => V.Vector a -> a
mean v = V.sum v / realToFrac (V.length v)

phaseAmp :: MonadIO m
         => V.Vector Double -> V.Vector Double -> TrackerT m (Phase, Amplitude)
phaseAmp exc samples = do
    let samples' = fmap (\x->x - mean samples) samples
        sampleLen = 10 * V.length exc
        corr = [ (i, correlate (V.take sampleLen samples') exc i)
               | i <- [0..V.length exc]
               ]
        (phase, corrNorm) = maximumBy (comparing snd) corr
        amp = corrNorm / norm exc
    plotSVG "corr.svg" $ plotPoints corr
    return $ (phase, amp)
    
plotSVG :: (ToRenderable a, MonadIO m) => FilePath -> a -> m ()
plotSVG fname a = liftIO $ renderableToSVGFile (toRenderable a) 640 480 fname

plotPoints :: (PlotValue x, PlotValue y) => [(x,y)] -> Layout1 x y
plotPoints pts = layout1_plots .~ [Left plot] $ def           
    where plot = toPlot $ plot_points_values .~ pts $ def
