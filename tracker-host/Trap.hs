{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Trap
    ( start
    , ParticleFoundCriterion
    , TrapConfig (..)
    , TrapActions (..)
    ) where

import Prelude
import Control.Monad (forever, when, void)
import Control.Monad.Trans.State
import Data.Int
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async
import Control.Concurrent.STM

import Data.Vector (Vector)
import qualified Data.Vector as V

import System.Posix.IO (openFd, closeFd, defaultFileFlags, OpenMode (..))
import System.FilePath ((</>), takeDirectory)
import System.Directory (createDirectoryIfMissing)

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

type ParticleFoundCriterion = Vector (T.Psd (T.SumDiff Int16)) -> Bool

data TrapConfig = TrapC
    { bleached      :: BinCount -> Bool
    , foundParticle :: ParticleFoundCriterion
    , binWidth      :: Time
    , setExcitation :: Switch
    , setTrap       :: Switch
    , searchScan    :: [T.Stage Int32]
    , outputFiles   :: [FilePath]
    }

data TrapActions = TrapA
    { trapStop :: IO ()
    , trapNext :: IO ()
    }

type Switch = MonadIO m => Bool -> m ()

printError :: MonadIO m => EitherT String m () -> m ()
printError action = do
    runEitherT action >>= either (liftIO . putStrLn) return

watchBins :: Time -> TChan BinCount -> IO ()
watchBins binWidth counts = printError $ do
    tt <- TT.open "/tmp/timetag.sock"
    liftIO $ runSafeT $ runEffect
             $ monitor tt "trapping"
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

data TrapState = TrapS { _scanPoints  :: [T.Stage Int32]
                       , _outFiles    :: [FilePath]
                       }
makeLenses ''TrapState

type TrapM = StateT TrapState (EitherT String (T.TrackerT IO))

run :: TrapConfig -> TMVar () -> TVar Bool -> Timetag -> TChan BinCount
    -> TrapM ()
run cfg nextVar stopVar tt counts = go
  where
    go = do
        findParticle cfg

        status "Start capture"
        outName : rest <- use outFiles
        outFiles .= rest
        liftIO $ createDirectoryIfMissing True (takeDirectory outName)
        dataFd <- liftIO $ openFd outName
                                  WriteOnly (Just 0644) defaultFileFlags
        output <- lift $ liftEitherIO $ TT.addOutputFd tt "trap_output" dataFd
        lift $ liftEitherIO $ TT.startCapture tt
        status "capture started"
        delayMillis 1000
        status "Set excitation"
        setExcitation cfg True

        void $ liftIO $ atomically $ tryTakeTMVar nextVar  -- ensure it's empty
        bleached <- liftIO $ async $ waitUntilBleached cfg counts
        next <- liftIO $ async $ atomically $ takeTMVar nextVar
        liftIO $ waitAnyCancel [bleached, next]

        status "Bleached"
        setExcitation cfg False
        status "Kill excitation"
        delayMillis 1000
        lift $ liftEitherIO $ TT.stopCapture tt
        lift $ liftEitherIO $ TT.removeOutput tt output
        status "Stop capture"

        setTrap cfg False
        advancePoints 10
        delayMillis 100
        setTrap cfg True
        status "Next"

        stop <- liftIO $ atomically $ readTVar stopVar
        when (not stop) go

    status :: String -> TrapM ()
    status = liftIO . putStrLn
    --status _ = return ()

start :: TrapConfig -> EitherT String (T.TrackerT IO) TrapActions
start cfg = do
    tt <- TT.open "/tmp/timetag.sock"
    counts <- liftIO newBroadcastTChanIO
    thread <- liftIO $ async $ watchBins (binWidth cfg) counts
    stopVar <- liftIO $ newTVarIO False
    nextVar <- liftIO newEmptyTMVarIO

    T.setKnob T.stageSetpoint zero
    T.setKnob T.feedbackMode T.StageFeedback
    T.setKnob T.adcDecimation 2
    T.startAdcStream
    T.setKnob T.adcTriggerMode T.TriggerAuto
    let s = TrapS { _scanPoints = cycle $ searchScan cfg
                  , _outFiles = outputFiles cfg
                  }
    thrd <- lift $ T.liftThrough async $ printError
            $ void $ runStateT (run cfg nextVar stopVar tt counts) s

    return $ TrapA
        { trapNext = atomically $ putTMVar nextVar ()
        , trapStop = atomically $ do writeTVar stopVar True
                                     putTMVar nextVar ()
        }

waitUntilBleached :: MonadIO m => TrapConfig -> TChan BinCount -> m ()
waitUntilBleached cfg countsChan = liftIO $ do
    putStrLn "Waiting until bleached"
    ch <- atomically $ dupTChan countsChan
    let go = do
            count <- atomically $ readTChan ch
            putStrLn $ "bin count = "++show count
            when (not $ bleached cfg count) go
    go

advancePoints :: Int -> TrapM ()
advancePoints n = do
    pts <- use scanPoints
    let p:rest = drop n pts
    scanPoints .= rest
    liftIO $ putStrLn $ "Position = "++show p
    lift $ T.setKnob T.stageSetpoint p

findParticle :: TrapConfig -> TrapM ()
findParticle cfg = do
    let go = do
            advancePoints 1
            queue <- lift $ lift T.getSensorQueue
            s <- liftIO $ atomically $ readTChan queue
            let psd = V.map (^. T.psd) s
            when (not $ foundParticle cfg psd) go
    go

liftEitherIO :: MonadIO m => EitherT e IO a -> EitherT e m a
liftEitherIO m = liftIO (runEitherT m) >>= EitherT . return

delayMillis :: MonadIO m => Int -> m ()
delayMillis n = liftIO $ threadDelay (n*1000)
