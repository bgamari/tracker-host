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

import Data.Time.Clock
import Data.Time.Format
import System.IO

import System.FilePath (takeDirectory)
import System.Directory (createDirectoryIfMissing)

import Control.Error
import Control.Lens
import Linear
import Pipes
import Pipes.Safe
import qualified Pipes.Prelude as PP
import qualified Pipes.ByteString as PBS

import qualified System.ZMQ4 as ZMQ

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

tryIO' :: MonadIO m => IO a -> EitherT String m a
tryIO' = fmapLT show . tryIO

printError :: MonadIO m => EitherT String m () -> m ()
printError action = do
    res <- runEitherT action
    case res of
      Left e  -> liftIO $ putStrLn $ "error: Trap: "++e
      Right _ -> return ()

watchBins :: Time -> TChan BinCount -> ZMQ.Context -> IO ()
watchBins _binWidth counts ctx = printError $ do
    liftIO $ runSafeT $ runEffect
             $ void (monitor ctx)
           >-> unwrapRecords
           >-> PP.map snd
           >-> binRecords _binWidth
           >-> toTChan counts
  where
    toTChan :: MonadIO m => TChan a -> Consumer a m r
    toTChan chan =
      forever $ await >>= liftIO . atomically . writeTChan chan

binRecords :: MonadIO m => Time -> Pipe Time Int m r
binRecords _binWidth = go 0 0
  where
    go count bin = do
        t <- await
        let b = t `div` _binWidth
        case () of
          _ | b == bin  -> go (count+1) bin
          _ | otherwise -> do yield count
                              go 0 b

data TrapState = TrapS { _scanPoints  :: [T.Stage Int32]
                       , _outFiles    :: [FilePath]
                       }
makeLenses ''TrapState

type TrapM = StateT TrapState (EitherT String (T.TrackerT IO))

run :: TrapConfig
    -> TMVar ()    -- ^ Used to force advance to next particle
    -> TVar Bool   -- ^ Set to terminate
    -> Handle      -- ^ Log
    -> Timetag     -- ^ Timetagger
    -> TChan BinCount
    -> ZMQ.Context
    -> TrapM ()
run cfg nextVar stopVar log tt counts ctx = go
  where
    go = do
        findParticle cfg

        status "Start capture"
        outName : rest <- use outFiles
        outFiles .= rest
        lift $ tryIO' $ createDirectoryIfMissing True (takeDirectory outName)
        outHandle <- lift $ tryIO' $ openFile outName WriteMode
        output <- lift $ tryIO' $ async $ runSafeT $ runEffect
                       $ rawRecords ctx >-> PBS.toHandle outHandle
        lift $ liftEitherIO $ TT.startCapture tt
        status "capture started"
        delayMillis 1000
        status "Set excitation"
        setExcitation cfg True

        void $ liftIO $ atomically $ tryTakeTMVar nextVar  -- ensure it's empty
        _bleached <- liftIO $ async $ waitUntilBleached cfg counts
        _next <- liftIO $ async $ atomically $ takeTMVar nextVar
        _ <- liftIO $ waitAnyCancel [_bleached, _next]

        status "Bleached"
        setExcitation cfg False
        status "Kill excitation"
        delayMillis 1000
        lift $ liftEitherIO $ TT.stopCapture tt
        lift $ liftIO $ cancel output
        status "Stop capture"

        setTrap cfg False
        advancePoints 100
        delayMillis 500
        setTrap cfg True
        status "Next"

        stop <- liftIO $ atomically $ readTVar stopVar
        when (not stop) go

    status :: String -> TrapM ()
    status msg = do
        t <- liftIO $ getCurrentTime
        let fmt = iso8601DateFormat (Just "%H:%M:%S")
            text = formatTime defaultTimeLocale fmt t++": "++msg
        liftIO $ hPutStrLn log text

start :: TrapConfig -> EitherT String (T.TrackerT IO) TrapActions
start cfg = do
    tt <- TT.open
    ctx <- liftIO ZMQ.context
    counts <- liftIO newBroadcastTChanIO
    watchThread <- liftIO $ async $ watchBins (binWidth cfg) counts ctx
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

    log <- tryIO' $ openFile "trap.log" WriteMode
    thrd <- lift $ T.liftThrough async $ printError $ void
            $ runStateT (run cfg nextVar stopVar log tt counts ctx) s

    return $ TrapA
        { trapNext = atomically $ putTMVar nextVar ()
        , trapStop = do atomically $ do writeTVar stopVar True
                                        putTMVar nextVar ()
                        wait thrd
                        cancel watchThread
                        void $ runEitherT $ TT.close tt
        }

waitUntilBleached :: MonadIO m => TrapConfig -> TChan BinCount -> m ()
waitUntilBleached cfg countsChan = liftIO $ do
    ch <- atomically $ dupTChan countsChan
    let go = do
            count <- atomically $ readTChan ch
            when (not $ bleached cfg count) go
    go

advancePoints :: Int -> TrapM ()
advancePoints n = do
    pts <- use scanPoints
    let p:rest = drop n pts
    scanPoints .= rest
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
