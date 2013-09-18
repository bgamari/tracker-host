{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable, DeriveGeneric #-}
module Tracker.RoughCal.Model where

import Prelude hiding (sequence)       
import Control.Applicative
import Data.Ord
import Data.Foldable
import Data.Traversable
import Data.Monoid
import qualified Data.Vector as V
import Linear
import Optimization.LineSearch.ConjugateGradient
import Tracker.Types
import GHC.Generics
import Numeric.AD

data Gaussian f a = Gaussian { gMean :: f a
                             , gVariance :: a
                             , gAmp :: a
                             }
                  deriving (Show, Functor, Foldable, Traversable, Generic)

instance Applicative f => Applicative (Gaussian f) where
    pure x = Gaussian (pure x) x x
    Gaussian m1 v1 a1 <*> Gaussian m2 v2 a2 =
        Gaussian (m1 <*> m2) (v1 v2) (a1 a2)
    
instance (Applicative f, Additive f) => Additive (Gaussian f) where
    zero = pure 0
instance (Applicative f, Metric f, Foldable f) => Metric (Gaussian f)

gaussian :: (RealFloat a, Metric f)
         => Gaussian f a -> f a -> a
gaussian (Gaussian m v a) x =
    a * exp (quadrance (m ^-^ x) / 2 / v)

data Model a = Model { g1, g2 :: Gaussian Stage a
                     , offset :: a
                     }
             deriving (Show, Functor, Foldable, Traversable, Generic)
     
instance Applicative Model where
    pure x = Model (pure x) (pure x) x
    Model a1 b1 c1 <*> Model a2 b2 c2 = Model (a1 <*> a2) (b1 <*> b2) (c1 c2)

instance Additive Model where
    zero = pure 0
instance Metric Model

model :: RealFloat a => Model a -> Stage a -> a
model (Model g1 g2 off) x =
    off + gaussian g1 x - gaussian g2 x

residual :: RealFloat a => Stage a -> a -> Model a -> a
residual x y m = model m x - y

mean :: Fractional a => V.Vector a -> a
mean xs = V.sum xs / fromIntegral (V.length xs)

initialModel :: (Ord a, Fractional a) => V.Vector (Stage a, a) -> Model a
initialModel samples =
    let (maxPos, maxAmp) = V.maximumBy (comparing snd) samples
        (minPos, minAmp) = V.minimumBy (comparing snd) samples
    in Model { g1 = Gaussian { gMean     = maxPos
                             , gVariance = 100
                             , gAmp      = maxAmp
                             }
             , g2 = Gaussian { gMean     = minPos
                             , gVariance = 100
                             , gAmp      = minAmp
                             }
             , offset = mean $ V.map snd samples
             }
 
fit :: RealFloat a => V.Vector (Stage a, a) -> Model a -> [Model a]
fit samples m0 = conjGrad search beta dChiSq m0
  where search = armijoSearch 0.1 0.2 1 chiSq
        beta = fletcherReeves
        dChiSq = grad chiSq
        chiSq :: RealFloat a => Model a -> a
        chiSq m = V.sum $ V.map (\(x,y)->residual (fmap realToFrac x) (realToFrac y) m) samples
