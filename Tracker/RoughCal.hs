{-# LANGUAGE RecordWildCards, RankNTypes, FlexibleContexts, UndecidableInstances,
             DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

module Tracker.RoughCal ( roughScan
                        , roughCenter
                        ) where

import Prelude hiding (product, sum, sequenceA, foldr)
import Data.Function (on)
import Control.Applicative
import qualified Data.Vector as V
import Data.Word
import Data.Foldable
import Data.Monoid
import Data.Traversable
import Control.Error
import Control.Lens
import Linear

import Tracker.Types
import Tracker.Raster
import Tracker.PathAcquire
import Tracker.LowLevel

roughScan :: MonadIO m
          => Word32 -> RasterScan Stage Word16
          -> EitherT String (TrackerT m) (V.Vector (Sensors Sample))
roughScan freq s =
    let s' = fmap realToFrac s :: RasterScan Stage Double
        path = map (fmap round) $ rasterScan sequenceStage s'
        sequenceStage s = (\x y z->Stage $ V3 x y z) <$> (s ^. _x) <*> (s ^. _y)  <*> (s ^. _z)
        --path = map (fmap round) $ rasterSine (realToFrac <$> _scanStart) (realToFrac <$> scanSize) (V3 1 10 40) 10000
    in pathAcquire freq path

roughCenter :: V.Vector (Sensors Word16) -> Stage Word16
roughCenter v =
    let psds :: Psd (V.Vector (SumDiff Word16))
        psds = sequenceA $ fmap (^. psd) v
        psdDiff :: Psd (V.Vector Word16)
        psdDiff = fmap (fmap (^. sdDiff)) psds
        maxPos :: Psd (Stage Word16)
        maxPos = mapped %~ ( fst
                           . maximumBy (compare `on` snd)
                           . V.zip (fmap (^. stage) v)
                           )
                $ psdDiff
        minPos :: Psd (Stage Word16)
        minPos = mapped %~ ( fst
                           . minimumBy (compare `on` snd)
                           . V.zip (fmap (^. stage) v)
                           )
                $ psdDiff
        hi = foldr (^+^) zero
             $ (^+^) <$> maxPos <*> minPos
            :: Stage Word16
    in (`div` 4) <$> hi
