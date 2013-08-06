{-# LANGUAGE GeneralizedNewtypeDeriving, TemplateHaskell #-}

module Tracker.Raster ( RasterScan(..), scanStart, scanSize, scanPoints
                      , scanAround
                      , rasterScan
                      , rasterSine
                      ) where

import Prelude hiding (maximum, any)
import Control.Applicative
import Data.Traversable
import Data.Foldable
import Linear
import Control.Lens
    
data RasterScan f a = RasterScan { _scanStart  :: f a
                                 , _scanSize   :: f a
                                 , _scanPoints :: f Int
                                 }
makeLenses ''RasterScan     

-- | Construct a scan around the given center with the given size           
scanAround :: (Additive f, Foldable f, Applicative f, Integral a, Ord a)
           => f a -> f a -> f Int -> Maybe (RasterScan f a)
scanAround center size npts
  | any id $ (>) <$> scanStart <*> scanEnd = Nothing
  | otherwise =
    Just $ RasterScan { _scanStart  = scanStart
                      , _scanSize   = size
                      , _scanPoints = npts
                      }
  where scanStart = center ^-^ halfSize
        scanEnd   = center ^+^ halfSize
        halfSize  = fmap (`div` 2) size


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
           => f a -> f a -> f Int -> [f a]
rasterScan start step npts =
    map (\n->start ^+^ ((*) <$> step <*> fmap realToFrac n))
    $ rasterScan' npts

rasterSine :: (RealFloat a, Foldable f, Applicative f)
           => f a -> f a -> f a -> Int -> [f a]
rasterSine center amp period n = 
    map (\i->f $ realToFrac i / realToFrac n * t) [0..n-1]
  where t = maximum period
        f t = (\c a p->c + a/2 * sin (2*pi*t/p)) <$> center <*> amp <*> period
