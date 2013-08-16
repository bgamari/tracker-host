{-# LANGUAGE TemplateHaskell, RankNTypes #-}                

module Tracker.FineCal ( fineCal
                       , FineScan(..)
                       , fineScanRange, fineScanCenter, fineScanPoints, fineScanFreq
                       ) where

import Data.Traversable as T
import Control.Applicative
import Control.Lens
import Control.Monad
import Data.List (foldl')
import Data.Word
import Data.Int
import qualified Data.Vector as V
import Linear
import System.Random.MWC
import Numeric.LinearAlgebra as LA

import Tracker.Types
import Tracker.Monad
import Tracker.PathAcquire

data FineScan = FineScan { _fineScanRange   :: Stage Word16
                         , _fineScanCenter  :: Stage Word16
                         , _fineScanPoints  :: Int
                         , _fineScanFreq    :: Word32
                         }
              deriving (Show)
makeLenses ''FineScan

isoMatrix :: (Element a)
          => f a -> [ReifiedLens' (f a) a]
          -> Iso' (V.Vector (f a)) (LA.Matrix a)
isoMatrix y0 projections = iso to from
  where proj = V.fromList projections
        to v = let f (i,j) = let p = reflectLens $ proj V.! j
                                 x = v V.! i
                             in x ^. p
               in LA.buildMatrix (V.length v) (V.length proj) f
        from m | LA.cols m == V.length proj =
                 let build = foldl' (\y1 (p,x)->y1 & reflectLens p .~ x) y0
                             . zip projections
                 in V.fromList $ map build $ LA.toLists m
               | otherwise = error "Incorret matrix size"
        
stagesToMatrix :: Element a => V.Vector (Stage a) -> LA.Matrix a
stagesToMatrix v = LA.buildMatrix (V.length v) 3 f
  where f (i,j) = case j of
                     0  -> x ^. _x
                     1  -> x ^. _y
                     2  -> x ^. _z
          where x = v V.! i

psdsToMatrix :: Element a => V.Vector (Psd (SumDiff a)) -> LA.Matrix a
psdsToMatrix v = LA.buildMatrix (V.length v) 4 f
  where f (i,j) = case j of
                     0  -> x ^. _x . sdSum
                     1  -> x ^. _x . sdDiff
                     2  -> x ^. _y . sdSum
                     3  -> x ^. _y . sdDiff
          where x = v V.! i

feedbackGainsFromMatrix :: Element a => LA.Matrix a -> Psd (Stage a)
feedbackGainsFromMatrix m =
    Psd $ V2 (Stage $ V3 (m LA.@@> (0,0)) (m LA.@@> (0,1)) (m LA.@@> (0,2)))
        (Stage $ V3 (m LA.@@> (1,0)) (m LA.@@> (1,1)) (m LA.@@> (1,2)))

thinSvd' :: (Field a, Element a) => V.Vector (Sensors a) -> Psd (Stage a)
thinSvd' xs = 
    let stages = stagesToMatrix $ V.map (^. stage) xs
        psds   = psdsToMatrix $ V.map (^. psd) xs
    in feedbackGainsFromMatrix $ LA.linearSolveLS stages psds
    
fineCal :: (Applicative m, MonadIO m)
        => FineScan -> TrackerT m (Psd (Stage Double))
fineCal fs = do
    points <- fineScan fs
    let ps' = fmap (fmap (realToFrac)) points
              :: V.Vector (Sensors Double)
    return $ thinSvd' ps'
   
fineScan :: (Applicative m, MonadIO m)
         => FineScan -> TrackerT m (V.Vector (Sensors Int16))
fineScan fs = do
    path <- liftIO $ withSystemRandom $ asGenIO $ \mwc->do
        let point = T.sequence $ pure (uniform mwc)
        replicateM (fs ^. fineScanPoints) (Stage <$> point)
    pathAcquire (fs^.fineScanFreq) path
