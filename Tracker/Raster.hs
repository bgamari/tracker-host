{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Tracker.Raster ( rasterScan
                      , rasterSine
                      ) where

import Prelude hiding (maximum)
import Control.Applicative
import Data.Traversable
import Data.Foldable
import Linear
    
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
