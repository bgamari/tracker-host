{-# LANGUAGE RankNTypes #-}

module MonitorTimetag where

import Control.Monad.IO.Class
import Control.Error
import Control.Lens
import System.Posix.IO.ByteString
import System.IO (Handle)

import Pipes
import Pipes.Safe
import Pipes.Binary
import Pipes.ByteString as PBS

import Timetag
import HPhoton.IO.FpgaTimetagger

failEitherT :: MonadThrow m => EitherT String m r -> m r
failEitherT m = do
    result <- runEitherT m
    case result of
      Left e -> fail e
      Right r -> return r

monitor :: Timetag -> OutputName -> Producer Record (SafeT IO) ()
monitor tt name = bracket start cleanup go
  where
    start = failEitherT $ do
        (readFd, writeFd) <- liftIO $ createPipe
        addOutputFd tt name writeFd
        liftIO $ closeFd writeFd
        liftIO $ fdToHandle readFd

    cleanup readH = failEitherT $ do
        removeOutput tt name

    go :: Handle -> Producer Record (SafeT IO) ()
    go readH = do
        res <- PBS.fromHandle readH ^. decoded
        case res of
          Left (e,_) -> fail $ show e
          Right _    -> return ()
