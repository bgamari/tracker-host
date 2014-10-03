{-# LANGUAGE OverloadedStrings #-}

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
import MonitorTimetag
import Timetag as TT
import HPhoton.IO.FpgaTimetagger.Pipes
import HPhoton.Types (Time)

type BinCount = Int

binWidth = round $ 1 * 128e6

type ParticleFoundCriterion = Vector (T.PsdChannels Int16) -> Bool
data TrapConfig = TrapC { bleached :: BinCount -> Bool
                        , foundParticle :: ParticleFoundCriterion
                        }

scan :: RasterScan T.Stage Double
scan = RasterScan { _scanCenter = zero
                  , _scanSize   = T.mkStage 30000 30000 0
                  , _scanPoints = T.mkStage 100 100 1
                  }

setTrap :: Bool -> IO ()
setTrap on = callProcess "thorlabs-laser" [if on then "--on" else "--off"]

setExcitation :: Bool -> IO ()
setExcitation on = do
    let args = ["set", "-c1", if on then "--on" else "--off"]
    callProcess "aotf-config" args

stdDevFound :: Double -> ParticleFoundCriterion
stdDevFound s =
    (> s) . stdDev . V.map (^. (_Wrapped' . _x . T.sdSum . to realToFrac))

main = do
    tt <- TT.open "/tmp/timetag.sock"
    let mon = monitor tt "trapping"
    counts <- newBroadcastTChanIO :: IO (TChan BinCount)
    async $ runSafeT
          $ runEffect $ mon
                    >-> unwrapRecords
                    >-> PP.map snd
                    >-> binRecords binWidth
                    >-> toTChan counts

    let config = TrapC { bleached = (< 200)
                       , foundParticle = stdDevFound 10
                       }
    result <- T.withTracker $ runEitherT $ do
         T.setKnob T.feedbackMode T.StageFeedback
         T.setKnob T.stageSetpoint zero
         T.setKnob T.adcDecimation 2
         T.startAdcStream
         T.setKnob T.adcTriggerMode T.TriggerAuto
         runStateT (run config tt counts)
                   (cycle $ map (fmap round) $ rasterScan sequenceA scan)
    either print (const $ return ()) result
    return ()

liftEitherIO :: MonadIO m => EitherT e IO a -> EitherT e m a
liftEitherIO m = liftIO (runEitherT m) >>= EitherT . return

run :: TrapConfig -> Timetag -> TChan BinCount
    -> StateT [T.Stage Int32] (EitherT String (T.TrackerT IO)) r
run config tt counts = forever $ do
    findParticle config
    lift $ liftEitherIO $ TT.startCapture tt
    liftIO $ threadDelay $ 1000*1000
    liftIO $ setExcitation True
    liftIO (waitUntilBleached config counts)
    liftIO $ setExcitation False
    liftIO $ threadDelay $ 1000*1000
    lift $ liftEitherIO $ TT.stopCapture tt
    liftIO $ setTrap True
    liftIO $ threadDelay $ 1000*1000
    liftIO $ setTrap False

findParticle :: TrapConfig -> StateT [T.Stage Int32] (EitherT String (T.TrackerT IO)) ()
findParticle config = do
    let go = do
            p:rest <- get
            put rest
            liftIO $ putStrLn $ "Position = "++show p
            lift $ T.setKnob T.stageSetpoint p
            queue <- lift $ lift T.getSensorQueue
            s <- liftIO $ atomically $ readTChan queue
            let psd = V.map (^. (T.psd . _Unwrapped')) s
            when (not $ foundParticle config psd) go
    liftIO $ putStrLn "Searching for particle"
    go

waitUntilBleached :: TrapConfig -> TChan BinCount -> IO ()
waitUntilBleached config countsChan = do
    putStrLn "Waiting until bleached"
    ch <- atomically $ dupTChan countsChan
    let go = do
            count <- atomically $ readTChan ch
            putStrLn $ "bin count = "++show count
            when (not $ bleached config count) go
    go
    return ()

binRecords :: MonadIO m => Time -> Pipe Time Int m r
binRecords binWidth = go 0 0
  where
    go count bin = do
      t <- await
      liftIO $ print t
      if t `div` binWidth > bin
        then yield count >> go 0 (t `div` binWidth)
        else go (count+1) bin

toTChan :: MonadIO m => TChan a -> Consumer a m r
toTChan chan = forever $ await >>= liftIO . atomically . writeTChan chan
