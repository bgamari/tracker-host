module TrackerUI.Queue
    ( TrackerQueue
    , withTrackerQueue
    , enqueueTrackerQ
    , runTrackerQ
    , runTrackerEQ
    ) where

import Control.Monad.Trans.Either
import Control.Monad.IO.Class
import Control.Monad (forever)
import Control.Concurrent (forkIO, killThread)
import Control.Concurrent.STM

import Tracker (TrackerT, withTracker)

newtype TrackerQueue = TQ (TQueue (TrackerT IO ()))

withTrackerQueue :: MonadIO m => (TrackerQueue -> m a) -> m a
withTrackerQueue f = do
    queue <- liftIO $ newTQueueIO
    worker <- liftIO $ forkIO $ do
        ret <- withTracker $ forever $ do 
            action <- liftIO $ atomically $ readTQueue queue
            action
        case ret of 
            Left err -> print err
            Right _  -> return ()
    ret <- f (TQ queue)
    liftIO $ killThread worker
    return ret

enqueueTrackerQ :: MonadIO m
               => TrackerQueue -> TrackerT IO a
               -> m (TMVar a)
enqueueTrackerQ (TQ queue) action = liftIO $ do
    result <- newEmptyTMVarIO
    atomically $ writeTQueue queue $ do
        r <- action
        liftIO $ atomically $ putTMVar result r
    return result

runTrackerQ :: MonadIO m
            => TrackerQueue -> TrackerT IO a -> m a
runTrackerQ tq action =
    enqueueTrackerQ tq action >>= liftIO . atomically . takeTMVar

runTrackerEQ :: MonadIO m
            => TrackerQueue -> EitherT e (TrackerT IO) a
            -> EitherT e m a
runTrackerEQ tq action =
    EitherT $ runTrackerQ tq (runEitherT action)
