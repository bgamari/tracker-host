module Tracker.PathAcquire ( pathAcquire ) where

import Control.Applicative
import Control.Monad (when, liftM)
import Data.Maybe (fromMaybe)
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

pathAcquire :: MonadIO m => Word32 -> [Stage Word16]
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

queuePoints :: MonadIO m => [Stage Word16] -> TrackerT m Bool
queuePoints points = go
  where go = do r <- enqueuePoints $ V.fromList points
                case r of
                  Just running -> return running
                  Nothing      -> liftIO (threadDelay 10000) >> go

primePath :: MonadIO m => [[Stage Word16]] -> TrackerT m [[Stage Word16]]
primePath (points:rest) = do
    r <- enqueuePoints $ V.fromList points
    case r of
      Just running -> if running then error "primePath: Attempted to prime while already running"
                                 else primePath rest
      Nothing      -> return rest

readFrames :: [V.Vector (Sensors Sample)]
           -> TrackerT IO (V.Vector (Sensors Sample))
readFrames accum = do
    d <- readData
    case d of
        Just d' -> readFrames (parseFrames d' : accum)
        Nothing -> return $ V.concat (reverse accum)
