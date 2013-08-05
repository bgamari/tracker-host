{-# LANGUAGE RecordWildCards #-}                

module Tracker
    ( open
    , close
    , readData
    , roughScan
    , module Tracker.Types
    , module Tracker.Commands
    ) where

import Prelude hiding (mapM_)
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

data RasterScan = RasterScan { scanStart  :: V3 Word16
                             , scanSize   :: V3 Word16
                             , scanPoints :: V3 Int
                             , scanFreq   :: Word32
                             }
                deriving (Show, Eq)
                
maxPathPoints = 80 :: Int

batchBy :: Int -> [a] -> [[a]]
batchBy _ [] = []
batchBy n xs = batch : batchBy n rest
  where (batch,rest) = splitAt n xs

roughScan :: Tracker -> RasterScan -> IO (V.Vector (Stage Sample, Psd Sample))
roughScan t (RasterScan {..}) = do
    setFeedbackMode t NoFeedback
    setAdcTriggerMode t TriggerManual
    let step = ((/) <$> fmap realToFrac scanSize <*> fmap realToFrac scanPoints)
        ps0:points = batchBy maxPathPoints
                     $ map (fmap round)
                     $ rasterScan (realToFrac <$> scanStart) step scanPoints
    queuePoints ps0 -- Ensure first batch of points make it out
    async $ mapM_ queuePoints $ points
    startAdcStream t
    framesAsync <- async $ readFrames []
    startPath t
    frames <- wait framesAsync
    stopAdcStream t
    setAdcTriggerMode t TriggerOff
    return frames
  where queuePoints :: [V3 Word16] -> IO ()
        queuePoints = untilTrue . enqueuePoints t . V.fromList

        untilTrue :: IO Bool -> IO ()
        untilTrue m = m >>= \success->when (not success) $ untilTrue m

        readFrames :: [V.Vector Frame] -> IO (V.Vector (Stage Sample, Psd Sample))
        readFrames frames = do
            d <- readData t
            case d of 
                Just d' -> readFrames (parseFrames d' : frames)
                Nothing -> return $ V.concat (reverse frames)
