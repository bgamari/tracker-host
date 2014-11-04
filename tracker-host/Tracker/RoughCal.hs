{-# LANGUAGE RecordWildCards, RankNTypes, FlexibleContexts, UndecidableInstances,
             DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

module Tracker.RoughCal ( rasterScanToPath
                        , roughScan
                        , roughCenter
                        ) where

import Prelude hiding (product, sum, foldr)
import Control.Applicative
import qualified Data.Vector as V
import Data.Word
import Data.Ord (comparing)
import Control.Error
import Control.Lens
import Linear
import Data.Functor.Rep

import Tracker.Types
import Tracker.Raster
import Tracker.PathAcquire
import Tracker.LowLevel
--import Tracker.RoughCal.Model

rasterScanToPath :: RasterScan Stage Word16 -> [Stage Word16]
rasterScanToPath = map (fmap round) . rasterScan sequenceStage . fmap toDouble
    where
      toDouble a = realToFrac a :: Double
      sequenceStage s = (\z x y -> Stage $ V3 x y z) <$> (s ^. _z) <*> (s ^. _x) <*> (s ^. _y)

roughScan :: MonadIO m
          => Word32 -> [Stage Word16]
          -> EitherT String (TrackerT m) (V.Vector (Sensors Sample))
roughScan freq path = pathAcquire freq path concatenatingM

center :: (Fractional a, Ord b) => V.Vector (Stage a, b) -> Stage a
center v =
    let (minPos,_) = V.minimumBy (comparing snd) v
        (maxPos,_) = V.maximumBy (comparing snd) v
    in lerp 0.5 minPos maxPos

asDouble :: Real a => a -> Double
asDouble = realToFrac

roughCenter :: V.Vector (Sensors Sample) -> Stage Double
roughCenter v =
    let Psd (V2 cx cy) = fmap center $ tabulate
                         $ \l->V.map (\s->( fmap asDouble $ s ^. stage
                                          , s ^. psd . el l . sdDiff)
                                     ) v
    in lerp 0.5 cx cy
