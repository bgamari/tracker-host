{-# LANGUAGE RecordWildCards #-}                

module Tracker
    ( TrackerT
    , withTracker
    , readData
    , scanAround
    , roughScan
    , RasterScan(..)
    , module Tracker.Types
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

import Tracker.Monad
import Tracker.Types
import Tracker.LowLevel
import Tracker.Commands
import Tracker.Raster

getInt16le :: Get Int16
getInt16le = fromIntegral `fmap` getWord16le

parseSamples :: BS.ByteString -> V.Vector Sample
parseSamples a = 
    runGet (V.replicateM (BS.length a `div` 2) getInt16le) $ BSL.fromStrict a

parseFrames :: BS.ByteString -> V.Vector (Stage Sample, Psd Sample)
parseFrames a = 
    runGet (V.replicateM (BS.length a `div` 32) frame) $ BSL.fromStrict a
  where frame = do stage <- sequenceA $ pure getInt16le :: Get (Stage Sample)
                   psd <- sequenceA $ pure getInt16le   :: Get (Psd Sample)
                   _ <- getInt16le
                   return (stage, psd)

scanAround :: V3 Word16 -> V3 Word16 -> V3 Int -> Maybe RasterScan
scanAround center size npts
  | any id $ (>) <$> scanStart <*> scanEnd = Nothing
  | otherwise =
    Just $ RasterScan { scanStart  = scanStart
                      , scanSize   = size
                      , scanPoints = npts
                      }
  where scanStart = center ^-^ halfSize
        scanEnd   = center ^+^ halfSize
        halfSize  = fmap (`div` 2) size
            
data RasterScan = RasterScan { scanStart  :: V3 Word16
                             , scanSize   :: V3 Word16
                             , scanPoints :: V3 Int
                             }
                deriving (Show, Eq)

batchBy :: Int -> [a] -> [[a]]
batchBy _ [] = []
batchBy n xs = batch : batchBy n rest
  where (batch,rest) = splitAt n xs

roughScan :: MonadIO m
          => Word32 -> RasterScan -> TrackerT m (V.Vector (Stage Sample, Psd Sample))
roughScan freq (RasterScan {..}) =
    let step = ((/) <$> fmap realToFrac scanSize <*> fmap realToFrac scanPoints)
        path = map (fmap round)
               $ rasterScan (realToFrac <$> scanStart) step scanPoints
               -- $ rasterSine (realToFrac <$> scanStart) (realToFrac <$> scanSize) (V3 1 10 40) 10000
    in pathAcquire freq path

pathAcquire :: MonadIO m => Word32 -> [V3 Word16]
            -> TrackerT m (V.Vector (Stage Sample, Psd Sample))
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

        readFrames :: [V.Vector Frame] -> TrackerT IO (V.Vector (Stage Sample, Psd Sample))
        readFrames frames = do
            d <- readData
            case d of 
                Just d' -> readFrames (parseFrames d' : frames)
                Nothing -> return $ V.concat (reverse frames)
