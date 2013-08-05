module Tracker
    ( open
    , close
    , readData
    , roughScan
    , module Tracker.Types
    , module Tracker.Commands
    ) where

import Data.Int    
import Data.Binary.Get
import qualified Data.Vector as V    
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Traversable       
import Control.Applicative
import Control.Concurrent.Async

import Tracker.Types
import Tracker.LowLevel
import Tracker.Commands

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

roughScan :: Tracker -> RasterScan -> IO (V.Vector (Stage Sample, Psd Sample))
roughScan t rscan = do
    setFeedbackMode t NoFeedback
    setAdcTriggerMode t TriggerManual
    startAdcStream t
    framesAsync <- async $ readFrames []
    rasterScan t rscan
    frames <- wait framesAsync
    stopAdcStream t
    setAdcTriggerMode t TriggerOff
    return frames
  where readFrames :: [V.Vector Frame] -> IO (V.Vector (Stage Sample, Psd Sample))
        readFrames frames = do
            d <- readData t
            case d of 
                Just d' -> readFrames (parseFrames d' : frames)
                Nothing -> return $ V.concat (reverse frames)
