{-# LANGUAGE GeneralizedNewtypeDeriving, StandaloneDeriving #-}

module Tracker.Monad ( TrackerT
                     , withDevice
                     , withDeviceIO
                     , runTrackerT
                     , liftThrough
                       -- * Convenient re-exports
                     , MonadIO
                     ) where

import Control.Monad.Reader
import Control.Monad.IO.Class
import Control.Applicative
import System.Console.Haskeline.MonadException
import System.USB

newtype TrackerT m a = TrackerT (ReaderT DeviceHandle m a)
                     deriving ( Functor, Applicative, Monad , MonadIO, MonadTrans )

deriving instance MonadException m => MonadException (TrackerT m)

withDevice :: Monad m => (DeviceHandle -> m a) -> TrackerT m a
withDevice f = TrackerT $ ask >>= lift . f

withDeviceIO :: MonadIO m => (DeviceHandle -> IO a) -> TrackerT m a
withDeviceIO f = TrackerT ask >>= liftIO . f

runTrackerT :: TrackerT m a -> DeviceHandle -> m a
runTrackerT (TrackerT m) h = runReaderT m h
            
-- | This is required for async which is monomorphic in IO            
liftThrough :: MonadIO m => (IO a -> IO b) -> TrackerT IO a -> TrackerT m b
liftThrough f (TrackerT a) = withDeviceIO $ f . runReaderT a
