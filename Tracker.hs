{-# LANGUAGE RecordWildCards #-}                

module Tracker
    ( -- * Tracker monad
      TrackerT
    , withTracker
      -- * Types
    , module Tracker.Types
    , Sensors(..)
      -- * Raster scanning path calculation
    , RasterScan(..)
    , scanCenter, scanStart, scanSize, scanPoints
      -- * Hardware commands
    , module Tracker.Commands
      -- * Input abstraction
    , module Tracker.Sensors
      -- * Rough feedback calibration
    , module Tracker.RoughCal
      -- * Fine feedback calibration
    , module Tracker.FineCal
      -- * Acquire raw sensor samples
    , adcAcquire
    ) where

import Tracker.Monad
import Tracker.Types
import Tracker.Commands
import Tracker.Raster
import Tracker.Sensors
import Tracker.RoughCal
import Tracker.FineCal
import Tracker.LowLevel

import qualified Data.Vector as V

adcAcquire :: MonadIO m => (Sensors Sample -> TrackerT m Bool) -> TrackerT m ()
adcAcquire onSample = startAdcStream >> go V.empty
  where go v
          | V.null v  = do d <- readData
                           case d of
                             Just d' -> go (parseFrames d')
                             Nothing -> go v
          | otherwise = do continue <- onSample (V.head v)
                           if continue then go (V.tail v)
                                       else stopAdcStream

