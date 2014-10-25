{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}

module Trap.MonitorTimetag where

import Control.Monad (forever)
import Control.Monad.IO.Class
import Control.Lens

import Pipes
import Pipes.Safe
import Pipes.Binary

import qualified System.ZMQ4 as ZMQ

import HPhoton.IO.FpgaTimetagger

monitor :: ZMQ.Context
        -> Producer Record (SafeT IO) (Either (DecodingError, Producer ByteString (SafeT IO) ()) ())
monitor ctx = rawRecords ctx ^. decoded

rawRecords :: ZMQ.Context -> Producer ByteString (SafeT IO) ()
rawRecords ctx = bracket start ZMQ.close receiveSocket
  where
    start = do
        sock <- ZMQ.socket ctx ZMQ.Sub
        ZMQ.subscribe sock ""
        ZMQ.connect sock "ipc:///tmp/timetag-data"
        return sock

receiveSocket :: (ZMQ.Receiver a, MonadIO m)
              => ZMQ.Socket a -> Producer ByteString m r
receiveSocket socket = forever $ liftIO (ZMQ.receive socket) >>= yield
