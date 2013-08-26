module Tracker.PathAcquire ( pathAcquire ) where

import Control.Applicative
import Control.Monad (when, liftM)
import Data.Maybe (fromMaybe)
import Data.Word
import qualified Data.Vector as V
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async, wait)
import Control.Concurrent.STM ( TChan, atomically, tryReadTChan
                              , TVar, newTVarIO, writeTVar, readTVar)
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
    -- the firmware will enable triggering upon starting the path
    -- disable triggering so we only see samples from after this point
    setAdcTriggerMode TriggerOff
    clearPath
    -- Start capturing data
    running <- liftIO $ newTVarIO True
    queue <- getSensorQueue
    framesAsync <- liftIO $ async $ readAllTChan running queue
    -- First fill up path queue
    points <- primePath $ batchBy maxPathPoints path
    startPath freq False
    mapM_ queuePoints $ points
    waitUntilPathFinished
    liftIO $ atomically $ writeTVar running False
    frames <- liftIO $ wait framesAsync
    setAdcTriggerMode TriggerAuto
    return $ V.concat frames

waitUntilPathFinished :: MonadIO m => TrackerT m ()
waitUntilPathFinished = do
    done <- isPathRunning
    when (not done) $ liftIO (threadDelay 10000) >> waitUntilPathFinished

readAllTChan :: TVar Bool -> TChan a -> IO [a]
readAllTChan running c = go []
  where go xs = do threadDelay 1000
                   (a, running) <- atomically $ (,) <$> tryReadTChan c <*> readTVar running
                   case a of
                     Nothing
                       | not running -> return $ reverse xs
                       | otherwise   -> go xs
                     Just x  -> go (x:xs)

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

