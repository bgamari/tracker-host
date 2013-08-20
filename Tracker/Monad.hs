{-# LANGUAGE GeneralizedNewtypeDeriving, StandaloneDeriving, TemplateHaskell #-}

module Tracker.Monad ( TrackerT
                     , withDevice
                     , withDeviceIO
                     , runTrackerT
                     , liftThrough
                       -- * Sensor sample notifications
                     , SensorQueue
                     , getSensorQueue
                     , notifySensors
                       -- * Convenient re-exports
                     , MonadIO, liftIO
                     ) where

import Control.Monad.Morph
import Control.Monad.Reader
import Control.Monad.IO.Class
import Control.Applicative
import System.Console.Haskeline.MonadException
import System.USB

import Control.Concurrent.STM
import Control.Concurrent.Async

import qualified Data.Vector as V
import Data.Int
import Data.Maybe
import Control.Lens
       
import Tracker.Types

type SensorQueue = TChan (V.Vector (Sensors Int16))
       
data Env = Env { _device         :: DeviceHandle
               , _sensorThread   :: Async ()
               , _sensorQueue    :: SensorQueue
               }
makeLenses ''Env
     
newtype TrackerT m a = TrackerT (ReaderT Env m a)
                     deriving ( Functor, Applicative, Monad , MonadIO, MonadException )
        
instance MonadTrans TrackerT where
    lift = TrackerT . lift

getSensorQueue :: MonadIO m => TrackerT m SensorQueue
getSensorQueue = TrackerT (view sensorQueue) >>= liftIO . atomically . dupTChan

notifySensors :: MonadIO m => V.Vector (Sensors Int16) -> TrackerT m ()
notifySensors sensors = TrackerT $ do
    queue <- view sensorQueue
    liftIO $ atomically $ writeTChan queue sensors
    
withDevice :: Monad m => (DeviceHandle -> m a) -> TrackerT m a
withDevice f = TrackerT (view device) >>= lift . f

withDeviceIO :: MonadIO m => (DeviceHandle -> IO a) -> TrackerT m a
withDeviceIO f = TrackerT (view device) >>= liftIO . f

sensorListen :: SensorQueue -> IO ()
sensorListen = undefined

runTrackerT :: MonadIO m => TrackerT m a -> DeviceHandle -> m a
runTrackerT (TrackerT m) h = do
    queue <- liftIO $ newBroadcastTChanIO
    thread <- liftIO $ async $ sensorListen queue
    let env = Env h thread queue
    runReaderT m env
            
-- | This is required for async which is monomorphic in IO            
liftThrough :: MonadIO m => (IO a -> IO b) -> TrackerT IO a -> TrackerT m b
liftThrough f (TrackerT a) = TrackerT $ do
    r <- ask
    liftIO $ f $ runReaderT a r
    
instance MFunctor TrackerT where
    hoist f (TrackerT m) = TrackerT $ ReaderT $ \r->f $ runReaderT m r
