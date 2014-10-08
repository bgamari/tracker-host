{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}

module Trap
    ( start
    , ParticleFoundCriterion
    , TrapConfig (..)
    , TrapEnv (..)
    , stdDevFound
    , setTrap
    , setExcitation
    ) where

import Prelude hiding (sequenceA)
import Control.Monad (forever, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.State
import Data.Traversable
import Data.Word
import Data.Int
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async
import Control.Concurrent.STM
import System.Process (callProcess)
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
import Tracker.Raster

import HPhoton.IO.FpgaTimetagger.Pipes
import HPhoton.Types (Time)

import Trap.MonitorTimetag
import Trap.Timetag as TT
import qualified Trap.Aotf

type BinCount = Int

type ParticleFoundCriterion = Vector (T.PsdChannels Int16) -> Bool

data TrapConfig = TrapC
    { bleached      :: BinCount -> Bool
    , foundParticle :: ParticleFoundCriterion
    , binWidth      :: Time
    }

type Switch = MonadIO m => Bool -> m ()

data TrapEnv = TrapE
    { setExcitation :: Switch
    , setTrap       :: Switch
    }

scan :: RasterScan T.Stage Double
scan = RasterScan
    { _scanCenter = zero
    , _scanSize   = T.mkStage 60000 60000 0
    , _scanPoints = T.mkStage 30 30 1
    }

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
      liftIO $ print t
      if t `div` binWidth > bin
        then yield count >> go 0 (t `div` binWidth)
        else go (count+1) bin

run :: TrapEnv -> TrapConfig -> Timetag -> TChan BinCount
    -> StateT [T.Stage Int32] (EitherT String (T.TrackerT IO)) r
run env config tt counts = forever $ do
    findParticle config

    lift $ liftEitherIO $ TT.startCapture tt
    delayMillis 1000
    setExcitation env True
    waitUntilBleached config counts
    setExcitation env False
    delayMillis 1000
    lift $ liftEitherIO $ TT.stopCapture tt

    setTrap env False
    advancePoints 10
    delayMillis 100
    setTrap env True

start :: TrapEnv -> TrapConfig -> EitherT String (T.TrackerT IO) ()
start env config = do
    tt <- liftIO $ TT.open "/tmp/timetag.sock"
    counts <- liftIO newBroadcastTChanIO
    thread <- liftIO $ async $ watchBins tt (binWidth config) counts

    T.setKnob T.stageSetpoint zero
    T.setKnob T.feedbackMode T.StageFeedback
    T.setKnob T.adcDecimation 2
    T.startAdcStream
    T.setKnob T.adcTriggerMode T.TriggerAuto
    runStateT (run env config tt counts)
              (cycle $ map (fmap round) $ rasterScan sequenceA scan)
    return ()

waitUntilBleached :: MonadIO m => TrapConfig -> TChan BinCount -> m ()
waitUntilBleached config countsChan = liftIO $ do
    putStrLn "Waiting until bleached"
    ch <- atomically $ dupTChan countsChan
    let go = do
            count <- atomically $ readTChan ch
            putStrLn $ "bin count = "++show count
            when (not $ bleached config count) go
    go
    return ()

advancePoints :: Int -> StateT [T.Stage Int32] (EitherT String (T.TrackerT IO)) ()
advancePoints n = do
    pts <- get
    let p:rest = drop n pts
    put rest
    liftIO $ putStrLn $ "Position = "++show p
    lift $ T.setKnob T.stageSetpoint p

findParticle :: TrapConfig -> StateT [T.Stage Int32] (EitherT String (T.TrackerT IO)) ()
findParticle config = do
    let go = do
            advancePoints 1
            queue <- lift $ lift T.getSensorQueue
            s <- liftIO $ atomically $ readTChan queue
            let psd = V.map (^. (T.psd . _Unwrapped')) s
            when (not $ foundParticle config psd) go
    liftIO $ putStrLn "Searching for particle"
    go

liftEitherIO :: MonadIO m => EitherT e IO a -> EitherT e m a
liftEitherIO m = liftIO (runEitherT m) >>= EitherT . return

delayMillis :: MonadIO m => Int -> m ()
delayMillis n = liftIO $ threadDelay (n*1000)
