module Tracker.PathAcquire ( pathAcquire ) where

import Control.Applicative
import Control.Monad (when)
import Data.Word
import qualified Data.Vector as V
import Control.Error
import Control.Monad.Trans
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async, wait)
import Control.Concurrent.STM ( TChan, atomically, tryReadTChan
                              , TVar, newTVarIO, writeTVar, readTVar)

import Tracker.LowLevel
import Tracker.Commands
import Tracker.Types

batchBy :: Int -> [a] -> [[a]]
batchBy _ [] = []
batchBy n xs = batch : batchBy n rest
  where (batch,rest) = splitAt n xs

pathAcquire :: MonadIO m => Word32 -> [Stage Word16]
            -> EitherT String (TrackerT m) (V.Vector (Sensors Sample))
pathAcquire _ [] = do
    liftIO $ putStrLn "pathAcquire: Tried to acquire on empty path"
    return V.empty
pathAcquire freq path = do
    mode <- getKnob feedbackMode
    setKnob feedbackMode StageFeedback
    -- the firmware will enable triggering upon starting the path
    -- disable triggering so we only see samples from after this point
    setKnob adcTriggerMode TriggerOff
    clearPath
    -- Start capturing data
    dec <- getKnob adcDecimation
    setKnob adcDecimation 1
    running <- liftIO $ newTVarIO True
    queue <- lift getSensorQueue
    framesAsync <- liftIO $ async $ readAllTChan running queue
    -- First fill up path queue and start running path
    points <- primePath $ batchBy maxPathPoints path
    startPath freq False
    mapM_ queuePoints $ points
    waitUntilPathFinished
    -- Restart ADC triggering
    setKnob feedbackMode mode
    setKnob adcDecimation dec
    setKnob adcTriggerMode TriggerAuto
    -- Grab remaining frames
    liftIO $ atomically $ writeTVar running False
    frames <- liftIO $ wait framesAsync
    return $ V.concat frames

waitUntilPathFinished :: MonadIO m => EitherT String (TrackerT m) ()
waitUntilPathFinished = do
    liftIO $ threadDelay 10000
    done <- isPathRunning
    when (not done) waitUntilPathFinished

readAllTChan :: TVar Bool -> TChan a -> IO [a]
readAllTChan runningVar c = go []
  where go xs = do threadDelay 1000
                   (a, running) <- atomically $ (,) <$> tryReadTChan c <*> readTVar runningVar
                   case a of
                     Nothing
                       | not running -> return $ reverse xs
                       | otherwise   -> go xs
                     Just x  -> go (x:xs)

queuePoints :: MonadIO m => [Stage Word16] -> EitherT String (TrackerT m) ()
queuePoints points = go
  where
    go = do (added, running) <- enqueuePoints $ V.fromList points
            when (not running) $ left "queuePoints: Path points underflowed"
            when (not added) $ liftIO (threadDelay 10000) >> go 

primePath :: MonadIO m
          => [[Stage Word16]] -> EitherT String (TrackerT m) [[Stage Word16]]
primePath [] = return []
primePath (points:rest) = do
    (added, running) <- enqueuePoints $ V.fromList points
    case (added, running) of
      (_, True)  -> left "primePath: Attempted to prime while already running"
      (True,  _) -> primePath rest
      (False, _) -> return rest
