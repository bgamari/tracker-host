{-# LANGUAGE RecordWildCards #-}                

module Tracker
    ( open
    , close
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
import Control.Applicative
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async

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

roughScan :: Tracker -> Word32 -> RasterScan -> IO (V.Vector (Stage Sample, Psd Sample))
roughScan t freq (RasterScan {..}) =
    let step = ((/) <$> fmap realToFrac scanSize <*> fmap realToFrac scanPoints)
        path = map (fmap round)
               $ rasterScan (realToFrac <$> scanStart) step scanPoints
    in pathAcquire t freq path

pathAcquire :: Tracker -> Word32 -> [V3 Word16]
            -> IO (V.Vector (Stage Sample, Psd Sample))
pathAcquire t freq path = do
    setFeedbackMode t NoFeedback
    setAdcTriggerMode t TriggerManual
    clearPath t
    -- First fill up path queue
    points <- primePath t $ batchBy maxPathPoints path
    startAdcStream t
    startPath t freq
    framesAsync <- async $ readFrames []
    mapM_ queuePoints $ points
    frames <- wait framesAsync
    stopAdcStream t
    setAdcTriggerMode t TriggerOff
    return frames
  where queuePoints :: [V3 Word16] -> IO ()
        queuePoints = untilTrue . enqueuePoints t . V.fromList

        primePath :: Tracker -> [[V3 Word16]] -> IO [[V3 Word16]]
        primePath t (points:rest) = do
            success <- enqueuePoints t $ V.fromList points
            if success then primePath t rest
                       else return rest


        untilTrue :: IO Bool -> IO ()
        untilTrue m = m >>= \success->when (not success) $ threadDelay 10000 >> untilTrue m

        readFrames :: [V.Vector Frame] -> IO (V.Vector (Stage Sample, Psd Sample))
        readFrames frames = do
            d <- readData t
            case d of 
                Just d' -> readFrames (parseFrames d' : frames)
                Nothing -> return $ V.concat (reverse frames)
