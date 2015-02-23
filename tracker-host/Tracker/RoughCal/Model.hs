{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable, DeriveGeneric #-}
module Tracker.RoughCal.Model where

import Prelude hiding (sequence)       
import Control.Applicative
import Data.Ord
import Data.Foldable
import Data.Traversable
import qualified Data.Vector as V
import Linear
import Optimization.LineSearch.ConjugateGradient
import Tracker.Types
import GHC.Generics
import Numeric.AD
import Control.Lens

data Gaussian f a = Gaussian { gMean :: !(f a)
                             , gStd  :: !a
                             , gAmp  :: !a
                             }
                  deriving (Show, Functor, Foldable, Traversable, Generic)

instance Applicative f => Applicative (Gaussian f) where
    pure x = Gaussian (pure x) x x
    Gaussian m1 d1 a1 <*> Gaussian m2 d2 a2 =
        Gaussian (m1 <*> m2) (d1 d2) (a1 a2)
    
instance (Applicative f, Additive f) => Additive (Gaussian f) where
    zero = pure 0
instance (Applicative f, Metric f, Foldable f) => Metric (Gaussian f)

gaussian :: (RealFloat a, Metric f)
         => Gaussian f a -> f a -> a
gaussian (Gaussian m d a) x =
    a * exp (- quadrance (m ^-^ x) / 2 / d^2)
{-# INLINE gaussian #-}

data Model f a = Model { g1, g2 :: !(Gaussian f a)
                       , offset :: !a
                       }
               deriving (Show, Functor, Foldable, Traversable, Generic)
     
instance Applicative f => Applicative (Model f) where
    pure x = Model (pure x) (pure x) x
    Model a1 b1 c1 <*> Model a2 b2 c2 = Model (a1 <*> a2) (b1 <*> b2) (c1 c2)

instance Applicative f => Additive (Model f) where
    zero = pure 0
instance (Foldable f, Applicative f) => Metric (Model f)

model :: (RealFloat a, Metric f)
      => Model f a -> f a -> a
model (Model g1 g2 off) x =
    off + gaussian g1 x + gaussian g2 x
{-# INLINE model #-}

residual :: (RealFloat a, Metric f) => f a -> a -> Model f a -> a
residual x y m = model m x - y
{-# INLINE residual #-}

mean :: Fractional a => V.Vector a -> a
mean xs = V.sum xs / fromIntegral (V.length xs)
{-# INLINE mean #-}

initialModel :: (Ord a, Fractional a) => V.Vector (V2 a, a) -> Model V2 a
initialModel samples =
    let (maxPos, maxAmp) = V.maximumBy (comparing snd) samples
        (minPos, minAmp) = V.minimumBy (comparing snd) samples
        offset = mean $ V.map snd samples
    in Model { g1 = Gaussian { gMean     = maxPos
                             , gStd      = 100
                             , gAmp      = maxAmp - offset
                             }
             , g2 = Gaussian { gMean     = minPos
                             , gStd      = 100
                             , gAmp      = minAmp - offset
                             }
             , offset = offset
             }
{-# INLINE initialModel #-}
 
-- Gradient by finite difference
finiteDiff :: (Fractional a, Additive f, Applicative f, Traversable f)
           => a -> (f a -> a) -> (f a -> f a)
finiteDiff h f x = fmap (\y->(y-fx) / h) fdx
  where fx = f x
        fdx = f . (x ^+^) <$> scaled (pure h)
{-# INLINE finiteDiff #-}

fit :: (RealFloat a, Traversable f, Applicative f, Metric f)
    => V.Vector (f a, a) -> Model f a -> [Model f a]
fit samples m0 = conjGrad search beta dChiSq m0
  where --search = wolfeSearch 0.1 0.2 1e-4 0.9 chiSq
        search = armijoSearch 0.1 0.1 1e-4 chiSq
        beta = fletcherReeves
        dChiSq = grad chiSq
        --dChiSq = finiteDiff 1e-2 chiSq
        chiSq m = V.sum $ V.map (\(x,y)->(residual (fmap realToFrac x) (realToFrac y) m)^2) samples
{-# INLINE fit #-}
    
modelCenter :: Additive f => Model f Double -> f Double
modelCenter m = lerp 0.5 (gMean $ g1 m) (gMean $ g2 m)
{-# INLINEABLE modelCenter #-}

modelToGains :: Model V3 Double -> V3 Double -> Psd (Stage Double)
modelToGains m center =
    _x .~ Stage (grad (model (fmap realToFrac m)) center)
  $ pure (pure 0)
