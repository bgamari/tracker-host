{-# LANGUAGE RecordWildCards #-}

module Tracker.RoughCal ( roughScan
                        , roughCenter
                        ) where

import Prelude hiding (product, sum)
import Control.Applicative
import qualified Data.Vector as V
import Data.Word
import Data.Foldable
import Data.Traversable
import Control.Lens
import Data.Distributive
import Linear

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

data Gaussian f a = Gaussian { mean :: !(f a)
                             , variance :: !(f a) -- | Diagonal covariance
                             }
                  deriving (Show, Eq)

data Model f a = Model { offset :: !a
                       , amp :: !a
                       , particle :: !(Gaussian f a)
                       }
               deriving (Show, Eq)

gaussian :: (Foldable f, Metric f, Applicative f, Distributive f, Traversable f, RealFloat a)
         => Gaussian f a -> f a -> a
gaussian (Gaussian {..}) x =
    1 / sqrt (2*pi) / detVar^2 * exp ((y *! invVar) `dot` y / 2)
  where y = x ^-^ mean
        invVar = kronecker $ fmap recip variance
        detVar = product variance
  
model :: (Foldable f, Metric f, Applicative f, Distributive f, Traversable f, RealFloat a)
      => Model f a -> f a -> a
model (Model {..}) x = amp * gaussian particle x + offset

guessModel :: RealFloat a => V.Vector (Stage a, a) -> Model Stage a
guessModel scan =
    Model { offset = snd $ V.head scan
          , amp = 100
          , particle = Gaussian { mean = pure 0x7fff
                                , variance = pure 0x1000
                                }
          }

residual :: (Num a) => V.Vector (f a, a) -> (f a -> a) -> a
residual v f = sum $ fmap (\(x,y)->(f x - y)^2) v

-- | Determine the center of the particle
roughCenter :: V.Vector (Sensors Sample) -> Stage Sample
roughCenter scan = undefined

minimize :: (f a -> a) -> f a -> [f a]
minimize = undefined
