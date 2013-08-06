module Tracker.RoughCal ( roughScan
                        , roughCenter
                        ) where
                        
import qualified Data.Vector as V    
import Data.Word    
import Control.Lens

import Tracker.Types
import Tracker.Raster
import Tracker.Sensors
import Tracker.Monad
import Tracker.PathAcquire
import Tracker.LowLevel

roughScan :: MonadIO m
          => Word32 -> RasterScan Stage Word16 -> TrackerT m (V.Vector (Sensors Sample))
roughScan freq s =
    let s' = fmap realToFrac s :: RasterScan Stage Double
        path = map (fmap round) $ rasterScan s'
        --path = map (fmap round) $ rasterSine (realToFrac <$> _scanStart) (realToFrac <$> scanSize) (V3 1 10 40) 10000
    in pathAcquire freq path

roughCenter :: V.Vector (Stage Sample, Psd Sample) -> Stage Sample
roughCenter scan = undefined
