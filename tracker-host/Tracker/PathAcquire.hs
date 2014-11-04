module Tracker.PathAcquire
    ( pathAcquire
    , concatenatingM
    ) where

import Control.Applicative
import Control.Monad (when, void)
import Data.Word
import qualified Data.Vector as V
import Control.Error
import Control.Monad.Trans
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async, wait)
import Control.Concurrent.STM ( TChan, tryReadTChan, TVar, writeTVar
                              , newTVarIO, atomically, readTVar)

import Pipes
import qualified Pipes.Prelude as PP
import qualified Control.Foldl as Foldl
import qualified Data.DList as DList

import Tracker.LowLevel
import Tracker.Commands
import Tracker.Types

batchBy :: Int -> [a] -> [[a]]
batchBy n = go
  where
    go [] = []
    go xs = batch : batchBy n rest
      where (batch,rest) = splitAt n xs

concatenatingM :: Monad m => Foldl.FoldM m (V.Vector a) (V.Vector a)
concatenatingM = Foldl.generalize concatenating

-- | A @Fold@ concatenating a sequence of vectors.
concatenating :: Foldl.Fold (V.Vector a) (V.Vector a)
concatenating = Foldl.Fold fold DList.empty unwrap
  where
    fold xs x = xs `DList.snoc` x
    unwrap = V.concat . DList.toList

pathAcquire :: MonadIO m
            => Word32 -> [Stage Word16]
            -> Foldl.FoldM IO (V.Vector (Sensors Sample)) b
            -> EitherT String (TrackerT m) b
pathAcquire _ [] _ = left "pathAcquire: Tried to acquire on empty path"
pathAcquire freq path consumer = do
    mode <- getKnob feedbackMode
    setKnob feedbackMode StageFeedback
    -- the firmware will enable triggering upon starting the path
    -- disable triggering so we only see samples from after this point
    setKnob adcTriggerMode TriggerOff
    flushAdcStream
    clearPath
    -- Start capturing data
    dec <- getKnob adcDecimation
    setKnob adcDecimation 1
    running <- liftIO $ newTVarIO True
    queue <- lift getSensorQueue
    consumerAsync <- liftIO $ async $ Foldl.impurely PP.foldM consumer
                                    $ readAllTChan running queue
    -- First fill up path queue
    points <- primePath $ batchBy maxPathPoints path
    -- The queue is full, start running path
    startPath freq False
    -- Queue remaining points as the queue drains
    mapM_ queuePoints points
    waitUntilPathFinished
    -- Restart ADC triggering
    setKnob feedbackMode mode
    setKnob adcDecimation dec
    setKnob adcTriggerMode TriggerAuto
    -- Grab remaining frames
    liftIO $ atomically $ writeTVar running False
    liftIO $ wait consumerAsync

waitUntilPathFinished :: MonadIO m => EitherT String (TrackerT m) ()
waitUntilPathFinished = do
    liftIO $ threadDelay 10000
    running <- isPathRunning
    when running waitUntilPathFinished

readAllTChan :: TVar Bool -> TChan a -> Producer a IO ()
readAllTChan runningVar inputChan = go
  where
    go = do
        (a, running) <- lift $ atomically $ (,) <$> tryReadTChan inputChan
                                                <*> readTVar runningVar
        case a of
          Nothing
            | not running -> return ()
            | otherwise   -> lift (threadDelay 1000) >> go
          Just x  -> yield x >> go


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
      (False, _) -> return (points:rest)
