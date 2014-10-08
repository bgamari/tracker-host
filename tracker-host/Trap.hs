{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}

module Trap
    ( start
    , ParticleFoundCriterion
    , TrapConfig (..)
    , stdDevFound
    ) where

import Prelude
import Control.Monad (forever, when)
import Control.Monad.Trans.State
import Data.Int
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async
import Control.Concurrent.STM
import Data.Vector (Vector)
import Statistics.Sample (stdDev)

import qualified Data.Vector as V
import Control.Error
import Control.Lens
import Linear
import Pipes
import Pipes.Safe
import qualified Pipes.Prelude as PP

import qualified Tracker.LowLevel as T
import qualified Tracker.Commands as T
import qualified Tracker.Types as T

import HPhoton.IO.FpgaTimetagger.Pipes
import HPhoton.Types (Time)

import Trap.MonitorTimetag
import Trap.Timetag as TT

type BinCount = Int

type ParticleFoundCriterion = Vector (T.PsdChannels Int16) -> Bool

data TrapConfig = TrapC
    { bleached      :: BinCount -> Bool
    , foundParticle :: ParticleFoundCriterion
    , binWidth      :: Time
    , setExcitation :: Switch
    , setTrap       :: Switch
    , searchScan    :: [T.Stage Int32]
    }

type Switch = MonadIO m => Bool -> m ()

stdDevFound :: Double -> ParticleFoundCriterion
stdDevFound s =
    (> s) . stdDev . V.map (^. (_Wrapped' . _x . T.sdSum . to realToFrac))

watchBins :: Timetag -> Time -> TChan BinCount -> IO ()
watchBins tt binWidth counts =
    runSafeT
    $ runEffect $ monitor tt "trapping"
               >-> unwrapRecords
               >-> PP.map snd
               >-> binRecords binWidth
               >-> toTChan counts
  where
    toTChan :: MonadIO m => TChan a -> Consumer a m r
    toTChan chan =
      forever $ await >>= liftIO . atomically . writeTChan chan

binRecords :: MonadIO m => Time -> Pipe Time Int m r
binRecords binWidth = go 0 0
  where
    go count bin = do
        t <- await
        let b = t `div` binWidth
        case () of
          _ | b == bin  -> go (count+1) bin
          _ | otherwise -> do yield count
                              go 0 b

type TrapM = StateT [T.Stage Int32] (EitherT String (T.TrackerT IO)) 

run :: TrapConfig -> Timetag -> TChan BinCount -> TrapM r
run cfg tt counts = forever $ do
    findParticle cfg

    lift $ liftEitherIO $ TT.startCapture tt
    delayMillis 1000
    setExcitation cfg True
    waitUntilBleached cfg counts
    setExcitation cfg False
    delayMillis 1000
    lift $ liftEitherIO $ TT.stopCapture tt

    setTrap cfg False
    advancePoints 10
    delayMillis 100
    setTrap cfg True

start :: TrapConfig -> EitherT String (T.TrackerT IO) ()
start cfg = do
    tt <- liftIO $ TT.open "/tmp/timetag.sock"
    counts <- liftIO newBroadcastTChanIO
    thread <- liftIO $ async $ watchBins tt (binWidth cfg) counts

    T.setKnob T.stageSetpoint zero
    T.setKnob T.feedbackMode T.StageFeedback
    T.setKnob T.adcDecimation 2
    T.startAdcStream
    T.setKnob T.adcTriggerMode T.TriggerAuto
    _ <- runStateT (run cfg tt counts) (cycle $ searchScan cfg)
    return ()

waitUntilBleached :: MonadIO m => TrapConfig -> TChan BinCount -> m ()
waitUntilBleached cfg countsChan = liftIO $ do
    putStrLn "Waiting until bleached"
    ch <- atomically $ dupTChan countsChan
    let go = do
            count <- atomically $ readTChan ch
            putStrLn $ "bin count = "++show count
            when (not $ bleached cfg count) go
    go
    return ()

advancePoints :: Int -> TrapM ()
advancePoints n = do
    pts <- get
    let p:rest = drop n pts
    put rest
    liftIO $ putStrLn $ "Position = "++show p
    lift $ T.setKnob T.stageSetpoint p

findParticle :: TrapConfig -> TrapM ()
findParticle cfg = do
    let go = do
            advancePoints 1
            queue <- lift $ lift T.getSensorQueue
            s <- liftIO $ atomically $ readTChan queue
            let psd = V.map (^. (T.psd . _Unwrapped')) s
            when (not $ foundParticle cfg psd) go
    liftIO $ putStrLn "Searching for particle"
    go

liftEitherIO :: MonadIO m => EitherT e IO a -> EitherT e m a
liftEitherIO m = liftIO (runEitherT m) >>= EitherT . return

delayMillis :: MonadIO m => Int -> m ()
delayMillis n = liftIO $ threadDelay (n*1000)
