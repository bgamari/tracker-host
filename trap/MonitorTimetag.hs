{-# LANGUAGE RankNTypes #-}

module MonitorTimetag where

import Control.Monad.IO.Class
import Control.Error
import Control.Lens
import System.Posix.IO.ByteString
import System.IO (Handle)

import qualified Data.ByteString as BS

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

monitor :: Timetag -> BS.ByteString -> Producer Record (SafeT IO) ()
monitor tt name = bracket start cleanup go
  where
    start = failEitherT $ do
        (readFd, writeFd) <- liftIO $ createPipe
        outputId <- addOutputFd tt name writeFd
        readH <- liftIO $ fdToHandle readFd
        return (readH, outputId)

    cleanup (_, outputId) = failEitherT $ do
        removeOutput tt outputId

    go :: (Handle, OutputId) -> Producer Record (SafeT IO) ()
    go (readH, _) = do
        res <- PBS.fromHandle readH ^. decoded
        case res of
          Left (e,_) -> fail $ show e
          Right _    -> return ()
