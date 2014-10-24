{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE BangPatterns #-}

module Trap.Timetag
    ( -- * Opening a connection
      Timetag
    , open
    , close
      -- * Managing capture state
    , startCapture
    , stopCapture
    , resetCounter
    , isCaptureRunning
    ) where

import Control.Error
import Control.Monad (void, when)
import Control.Monad.IO.Class
import Data.Monoid
import qualified System.ZMQ4 as ZMQ
import qualified Data.ByteString.Char8 as BS

data Timetag = Timetag ZMQ.Context (ZMQ.Socket ZMQ.Req)

tryIO' :: MonadIO m => IO a -> EitherT String m a
tryIO' = fmapLT show . tryIO

open :: MonadIO m => EitherT String m Timetag
open = tryIO' $ do
    ctx <- ZMQ.context
    s <- ZMQ.socket ctx ZMQ.Req
    ZMQ.connect s "ipc:///tmp/timetag-ctrl"
    return $ Timetag ctx s

close :: MonadIO m => Timetag -> EitherT String m ()
close (Timetag ctx s) = tryIO' $ do
    ZMQ.close s
    ZMQ.shutdown ctx

command :: BS.ByteString -> Timetag -> EitherT String IO BS.ByteString
command cmd tt@(Timetag _ s) = tryIO' $ do
    n <- ZMQ.send s [] cmd
    ZMQ.receive s

assertOk :: EitherT String IO BS.ByteString -> EitherT String IO ()
assertOk action = do
    res <- action
    if "ok" `BS.isPrefixOf` res
      then return ()
      else left $ "Expected 'ok', saw "++show res

startCapture :: Timetag -> EitherT String IO ()
startCapture = assertOk . command "start_capture"

stopCapture :: Timetag -> EitherT String IO ()
stopCapture = assertOk . command "stop_capture"

resetCounter :: Timetag -> EitherT String IO ()
resetCounter = assertOk . command "reset_counter"


isCaptureRunning :: Timetag -> EitherT String IO Bool
isCaptureRunning tt = do
    reply <- command "capture?\n" tt
    case () of
      () | reply == "1" -> return True
      _  | reply == "0" -> return False
      _  | otherwise    -> left $ "invalid reply "++show reply
