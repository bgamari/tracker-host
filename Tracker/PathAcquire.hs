module Tracker.PathAcquire ( pathAcquire ) where

import Control.Applicative
import Control.Monad (when, liftM)
import Data.Maybe (fromMaybe)
import Data.Word
import qualified Data.Vector as V
import Control.Concurrent (threadDelay)
import Control.Concurrent.STM (TChan, STM, atomically, tryReadTChan)
import Linear

import Tracker.LowLevel
import Tracker.Commands
import Tracker.Types

batchBy :: Int -> [a] -> [[a]]
batchBy _ [] = []
batchBy n xs = batch : batchBy n rest
  where (batch,rest) = splitAt n xs

pathAcquire :: MonadIO m => Word32 -> [Stage Word16]
            -> TrackerT m (V.Vector (Sensors Sample))
pathAcquire _ [] = do
    liftIO $ putStrLn "pathAcquire: Tried to acquire on empty path"
    return V.empty
pathAcquire freq path = do
    setFeedbackMode NoFeedback
    setAdcTriggerMode TriggerOff
    clearPath
    -- First fill up path queue
    points <- primePath $ batchBy maxPathPoints path
    startPath freq False
    queue <- getSensorQueue
    mapM_ queuePoints $ points
    waitUntilPathFinished
    frames <- V.concat `liftM` liftIO (atomically $ readAllTChan queue)
    return frames

waitUntilPathFinished :: MonadIO m => TrackerT m ()
waitUntilPathFinished = do
    done <- isPathRunning
    when (not done) $ liftIO (threadDelay 10000) >> waitUntilPathFinished

readAllTChan :: TChan a -> STM [a]
readAllTChan c = go []
  where go xs = do a <- tryReadTChan c
                   case a of
                     Just x  -> go (x:xs)
                     Nothing -> return $ reverse xs

queuePoints :: MonadIO m => [Stage Word16] -> TrackerT m Bool
queuePoints points = go
  where go = do r <- enqueuePoints $ V.fromList points
                case r of
                  Just running -> return running
                  Nothing      -> liftIO (threadDelay 10000) >> go

primePath :: MonadIO m => [[Stage Word16]] -> TrackerT m [[Stage Word16]]
primePath [] = return []
primePath (points:rest) = do
    r <- enqueuePoints $ V.fromList points
    case r of
      Just running -> if running then error "primePath: Attempted to prime while already running"
                                 else primePath rest
      Nothing      -> return rest

