{-# LANGUAGE OverloadedStrings #-}

import Prelude hiding (sequenceA)
import Control.Monad (forever, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.State
import Data.Traversable
import Data.Word
import Data.Int
import Control.Concurrent (forkIO)
import Control.Concurrent.STM

import qualified Data.Vector as V
import Control.Error
import Control.Lens
import Linear
import Pipes
import Pipes.Safe

import qualified Tracker.LowLevel as T
import qualified Tracker.Commands as T
import qualified Tracker.Types as T
import Tracker.Raster
import MonitorTimetag
import Timetag as TT
import HPhoton.IO.FpgaTimetagger

type BinCount = Int

binWidth = round $ 1 * 128e6

data TrapConfig = TrapC { bleached :: BinCount -> Bool
                        , foundParticle :: T.PsdChannels Int16 -> Bool
                        }

scan :: RasterScan T.Stage Double
scan = RasterScan { _scanCenter = zero
                  , _scanSize   = T.mkStage 6000 6000 0
                  , _scanPoints = T.mkStage 100 100 1
                  }

main = do
    tt <- TT.open "/tmp/timetag.sock"
    let mon = monitor tt (OutputName "trapping")
    counts <- newBroadcastTChanIO :: IO (TChan BinCount)
    forkIO $ runSafeT $ runEffect $ mon >-> binRecords binWidth >-> toTChan counts
    let config = TrapC { bleached = (>200)
                       , foundParticle = const True
                       }
    result <- T.withTracker $ runEitherT $ do
         T.setKnob T.feedbackMode T.StageFeedback
         T.setKnob T.stageSetpoint zero
         runStateT (run config counts)
                   (cycle $ map (fmap round) $ rasterScan sequenceA scan)
    either print (const $ return ()) result
    return ()

run :: TrapConfig -> TChan BinCount
    -> StateT [T.Stage Int32] (EitherT String (T.TrackerT IO)) r
run config counts = forever $ do
    findParticle config
    liftIO (waitUntilBleached config counts)

findParticle :: TrapConfig -> StateT [T.Stage Int32] (EitherT String (T.TrackerT IO)) ()
findParticle config = do
    let go = do
            p:rest <- get
            put rest
            lift $ T.setKnob T.stageSetpoint p
            queue <- lift $ lift T.getSensorQueue
            s <- liftIO $ atomically $ readTChan queue
            when (not $ foundParticle config (s ^. to V.last . T.psd . _Unwrapped')) go
    liftIO $ putStrLn "Searching for particle"
    go

waitUntilBleached :: TrapConfig -> TChan BinCount -> IO ()
waitUntilBleached config countsChan = do
    putStrLn "Waiting until bleached"
    ch <- atomically $ dupTChan countsChan
    let go = do
            count <- atomically $ readTChan ch
            when (not $ bleached config count) go
    go
    return ()

binRecords :: Monad m => Word64 -> Pipe Record Int m r
binRecords binWidth = go 0 0
  where
    go count bin = do
      r <- await
      let t = r ^. recTime
      if t `div` binWidth > bin
        then yield count >> go 0 (t `div` binWidth)
        else go (count+1) bin

toTChan :: MonadIO m => TChan a -> Consumer a m r
toTChan chan = forever $ await >>= liftIO . atomically . writeTChan chan
