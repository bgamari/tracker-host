{-# LANGUAGE GeneralizedNewtypeDeriving, TemplateHaskell #-}

module Tracker.Raster ( RasterScan(..)
                      , scanStart, scanEnd, scanCenter, scanSize, scanPoints
                      , rasterScan
                      , rasterSine
                      ) where

import Prelude hiding (maximum, any)
import Control.Applicative
import Data.Traversable
import Data.Foldable
import Linear
import Control.Lens
    
data RasterScan f a = RasterScan { _scanCenter :: f a
                                 , _scanSize   :: f a
                                 , _scanPoints :: f Int
                                 }
makeLenses ''RasterScan

instance Functor f => Functor (RasterScan f) where
    fmap f (RasterScan c s p) = RasterScan (fmap f c) (fmap f s) p

scanStart :: (Additive f, Fractional a) => Getter (RasterScan f a) (f a)
scanStart = to $ \s->s^.scanCenter ^-^ s^.scanSize ^/ 2

scanEnd :: (Additive f, Fractional a) => Getter (RasterScan f a) (f a)
scanEnd = to $ \s->s^.scanCenter ^+^ s^.scanSize ^/ 2

newtype FlipList a = FList {getFlipList :: [a]}
                   deriving (Functor, Foldable, Traversable)

instance Applicative FlipList where
    pure x = FList [x]
    FList [] <*> _ = FList []
    FList (f:fs) <*> FList xs = FList $ map f xs ++ rest
      where FList rest = FList fs <*> FList (reverse xs)

rasterScan' :: (Traversable f) => f Int -> [f Int]
rasterScan' = getFlipList . traverse (\n->FList [0..n-1])

rasterScan :: (RealFrac a, Additive f, Applicative f, Traversable f)
           => RasterScan f a -> [f a]
rasterScan s =
    map (\n->s^.scanStart ^+^ ((*) <$> step <*> fmap realToFrac n))
    $ rasterScan' $ s^.scanPoints
  where step = (/) <$> s^.scanSize <*> fmap realToFrac (s^.scanPoints)

rasterSine :: (RealFloat a, Foldable f, Applicative f)
           => f a -> f a -> f a -> Int -> [f a]
rasterSine center amp period n = 
    map (\i->f $ realToFrac i / realToFrac n * t) [0..n-1]
  where t = maximum period
        f t = (\c a p->c + a/2 * sin (2*pi*t/p)) <$> center <*> amp <*> period
