{-# LANGUAGE RecordWildCards #-}                

module Tracker
    ( TrackerT
    , withTracker
    , scanAround
      -- Raster scanning
    , roughScan
    , RasterScan(..)
    , scanStart, scanSize, scanPoints
      -- * Types
    , module Tracker.Types
    , Sensors(..)
      -- * Hardware commands
    , module Tracker.Commands
      -- * Sensors
    , module Tracker.Sensors
    ) where

import Prelude hiding (mapM_, any)
import Data.Word    
import qualified Data.Vector as V    
import Data.Traversable       
import Data.Foldable       
import Linear
import Control.Monad (when)
import Control.Monad.IO.Class
import Control.Applicative
import Control.Lens

import Tracker.Monad
import Tracker.PathAcquire
import Tracker.Types
import Tracker.LowLevel
import Tracker.Commands
import Tracker.Raster
import Tracker.Sensors

roughScan :: MonadIO m
          => Word32 -> RasterScan Stage Word16 -> TrackerT m (V.Vector (Sensors Sample))
roughScan freq (RasterScan {..}) =
    let step = ((/) <$> fmap realToFrac _scanSize <*> fmap realToFrac _scanPoints)
        path = map (fmap round)
               $ rasterScan (realToFrac <$> _scanStart) step _scanPoints
               -- $ rasterSine (realToFrac <$> _scanStart) (realToFrac <$> scanSize) (V3 1 10 40) 10000
    in pathAcquire freq path

roughCenter :: V.Vector (Stage Sample, Psd Sample) -> Stage Sample
roughCenter scan = undefined

