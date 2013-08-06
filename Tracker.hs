{-# LANGUAGE RecordWildCards, TemplateHaskell #-}                

module Tracker
    ( TrackerT
    , withTracker
    , scanAround
      -- Raster scanning
    , roughScan
    , RasterScan(..)
    , scanStart, scanSize, scanPoints
      -- * Types
    , module Tracker.Types
    , Sensors(..)
      -- * Hardware commands
    , module Tracker.Commands
    ) where

import Prelude hiding (mapM_, any)
import Data.Word    
import Data.Int    
import Data.Binary.Get
import qualified Data.Vector as V    
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Traversable       
import Data.Foldable       
import Linear
import Control.Monad (when)
import Control.Monad.IO.Class
import Control.Applicative
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async
import Control.Lens

import Tracker.Monad
import Tracker.Types
import Tracker.LowLevel
import Tracker.Commands
import Tracker.Raster

getInt16le :: Get Int16
getInt16le = fromIntegral `fmap` getWord16le

parseFrames :: BS.ByteString -> V.Vector (Stage Sample, Psd (SumDiff Sample))
parseFrames a = 
    runGet (V.replicateM (BS.length a `div` 32) frame) $ BSL.fromStrict a
  where frame = do stage <- sequenceA $ pure getInt16le :: Get (Stage Sample)
                   xDiff <- getInt16le
                   yDiff <- getInt16le
                   xSum  <- getInt16le
                   ySum  <- getInt16le
                   _ <- getInt16le
                   return (stage, Psd $ V2 (SumDiff xSum xDiff) (SumDiff ySum yDiff))

sumDiffToPsd :: Num a => Psd (SumDiff a) -> Psd (Diode a)
sumDiffToPsd = fmap diode
  where diode :: Num a => SumDiff a -> Diode a
        diode (SumDiff sum diff) = Diode (sum - diff) (sum + diff)
            
data RasterScan = RasterScan { _scanStart  :: V3 Word16
                             , _scanSize   :: V3 Word16
                             , _scanPoints :: V3 Int
                             }
                deriving (Show, Eq)
makeLenses ''RasterScan     
  
scanAround :: V3 Word16 -> V3 Word16 -> V3 Int -> Maybe RasterScan
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

batchBy :: Int -> [a] -> [[a]]
batchBy _ [] = []
batchBy n xs = batch : batchBy n rest
  where (batch,rest) = splitAt n xs

data Sensors a = Sensors { stage :: !(Stage a)
                         , psd   :: !(Psd (Diode a))
                         }
               deriving (Show)
               
roughScan :: MonadIO m
          => Word32 -> RasterScan -> TrackerT m (V.Vector (Sensors Sample))
roughScan freq (RasterScan {..}) =
    let step = ((/) <$> fmap realToFrac _scanSize <*> fmap realToFrac _scanPoints)
        path = map (fmap round)
               $ rasterScan (realToFrac <$> _scanStart) step _scanPoints
               -- $ rasterSine (realToFrac <$> _scanStart) (realToFrac <$> scanSize) (V3 1 10 40) 10000
    in pathAcquire freq path

roughCenter :: V.Vector (Stage Sample, Psd Sample) -> Stage Sample
roughCenter scan = undefined

pathAcquire :: MonadIO m => Word32 -> [V3 Word16]
            -> TrackerT m (V.Vector (Sensors Sample))
pathAcquire freq path = do
    setFeedbackMode NoFeedback
    setAdcTriggerMode TriggerOff
    clearPath
    -- First fill up path queue
    points <- primePath $ batchBy maxPathPoints path
    startAdcStream
    startPath freq
    framesAsync <- liftThrough async $ readFrames []
    mapM_ queuePoints $ points
    frames <- liftIO $ wait framesAsync
    stopAdcStream
    return frames
  where queuePoints :: MonadIO m => [V3 Word16] -> TrackerT m ()
        queuePoints = untilTrue . enqueuePoints . V.fromList

        primePath :: MonadIO m => [[V3 Word16]] -> TrackerT m [[V3 Word16]]
        primePath (points:rest) = do
            success <- enqueuePoints $ V.fromList points
            if success then primePath rest
                       else return rest


        untilTrue :: MonadIO m => m Bool -> m ()
        untilTrue m = m >>= \success->when (not success)
                                      $ liftIO (threadDelay 10000) >> untilTrue m

        readFrames :: [V.Vector (Sensors Sample)]
                   -> TrackerT IO (V.Vector (Sensors Sample))
        readFrames accum = do
            d <- readData
            case d of 
                Just d' -> let d'' = V.map (\(stage,sumDiff)->Sensors stage (sumDiffToPsd sumDiff)) $ parseFrames d'
                           in readFrames (d'' : accum)
                Nothing -> return $ V.concat (reverse accum)
