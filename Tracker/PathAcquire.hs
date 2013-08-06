module Tracker.PathAcquire ( pathAcquire ) where

import Control.Monad (when)
import Data.Word
import qualified Data.Vector as V
import Control.Concurrent.Async
import Control.Concurrent (threadDelay)
import Linear

import Tracker.LowLevel
import Tracker.Sensors
import Tracker.Monad
import Tracker.Commands
import Tracker.Types

batchBy :: Int -> [a] -> [[a]]
batchBy _ [] = []
batchBy n xs = batch : batchBy n rest
  where (batch,rest) = splitAt n xs

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

queuePoints :: MonadIO m => [V3 Word16] -> TrackerT m ()
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
        Just d' -> readFrames (parseFrames d' : accum)
        Nothing -> return $ V.concat (reverse accum)
