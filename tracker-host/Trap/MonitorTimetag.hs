{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}

module Trap.MonitorTimetag where

import Control.Monad (forever)
import Control.Monad.IO.Class
import Control.Error
import Control.Lens

import qualified Data.ByteString as BS

import Pipes
import Pipes.Safe
import Pipes.Binary
import Pipes.ByteString as PBS

import qualified System.ZMQ4 as ZMQ

import HPhoton.IO.FpgaTimetagger

monitor :: Producer Record (SafeT IO) (Either (DecodingError, Producer ByteString (SafeT IO) ()) ())
monitor = rawRecords ^. decoded

rawRecords :: Producer ByteString (SafeT IO) ()
rawRecords = bracket start cleanup go
  where
    start = do
        ctx <- ZMQ.context
        sock <- ZMQ.socket ctx ZMQ.Sub
        ZMQ.subscribe sock ""
        ZMQ.connect sock "ipc:///tmp/timetag-data"
        return (ctx, sock)

    cleanup (ctx, sock) = do
        ZMQ.close sock
        ZMQ.shutdown ctx

    go (_, socket) = receiveSocket socket

receiveSocket :: (ZMQ.Receiver a, MonadIO m)
              => ZMQ.Socket a -> Producer ByteString m r
receiveSocket socket = forever $ liftIO (ZMQ.receive socket) >>= yield
