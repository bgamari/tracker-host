{-# LANGUAGE RecordWildCards, RankNTypes, FlexibleContexts, UndecidableInstances,
             DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

module Tracker.RoughCal ( roughScan
                        , roughCenter
                        ) where

import Prelude hiding (product, sum)
import Control.Applicative
import qualified Data.Vector as V
import Data.Word
import Data.Foldable
import Data.Monoid
import Data.Traversable
import Control.Lens
import Data.Distributive
import Linear
import Optimization.LineSearch
import Optimization.LineSearch.ConjugateGradient
import Numeric.AD
import Numeric.AD.Types
import Numeric.AD.Internal.Classes (Lifted)

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

data Gaussian a = Gaussian { mean, variance :: !a }
                deriving (Show, Eq, Functor, Foldable, Traversable)

instance Applicative Gaussian where
    pure x = Gaussian x x
    Gaussian a b <*> Gaussian x y = Gaussian (a x) (b y)
instance Additive Gaussian where zero = pure 0
instance Metric Gaussian

data Model f a = Model { offset :: !a
                       , amp :: !a
                       , particle :: !(Gaussian (f a))
                       }
               deriving (Show, Eq)

instance Functor f => Functor (Model f) where
    fmap f (Model o a p) = Model (f o) (f a) (fmap (fmap f) p)
instance Foldable f => Foldable (Model f) where
    foldMap f (Model o a p) = f o <> f a <> foldMap (foldMap f) p
instance Traversable f => Traversable (Model f) where
    traverse f (Model o a p) = Model <$> f o <*> f a <*> traverse (traverse f) p
instance Applicative f => Applicative (Model f) where
    pure x = Model x x (pure $ pure x)
    Model o a p <*> Model o' a' p' = Model (o o') (a a') (fmap (<*>) p <*> p')
instance Applicative (Model f) => Additive (Model f) where zero = pure 0
instance (Foldable f, Metric f, Applicative (Model f)) => Metric (Model f)

gaussian :: (Foldable f, Metric f, Applicative f, Distributive f, Traversable f, RealFloat a)
         => Gaussian (f a) -> f a -> a
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
roughCenter :: (RealFloat a) => V.Vector (Sensors a) -> Model Stage a
roughCenter scan =
    let x0 = guessModel fitData
        fitData = fmap (\s->(s^.stage, s^.psd^._x^.anode)) scan
        fitData' :: (Lifted s, RealFloat a) => V.Vector (Stage (AD s a), AD s a)
        fitData' = fmap (\(a,b)->(fmap realToFrac a, realToFrac b)) fitData
        beta = fletcherReeves
        search :: RealFloat a => LineSearch (Model Stage) a
        search = armijoSearch 0.1 1 2 (lowerFU f)
        f :: (Lifted s, RealFloat a) => Model Stage (AD s a) -> AD s a
        f m = residual fitData' $ model m
    in head $ drop 10 $ conjGrad search beta (grad f) (fmap realToFrac x0)
