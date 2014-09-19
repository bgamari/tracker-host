{-# LANGUAGE OverloadedStrings #-}

module Timetag
    ( Timetag
    , open
    , startCapture
    , stopCapture
    , resetCounter
    , isCaptureRunning
    , OutputName (..)
    , addOutputFd
    , removeOutput
    ) where

import Control.Error
import Control.Monad (void)
import Control.Monad.IO.Class
import Data.Monoid
import qualified Data.ByteString.Char8 as BS

import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString
import System.Posix.Types

newtype Timetag = Timetag Socket

open :: FilePath -> IO Timetag
open path = do
    s <- socket AF_UNIX Stream defaultProtocol
    connect s (SockAddrUnix path)
    --ready <- liftIO $ recv s 128
    recvUntil '\n' s >>= print
    return $ Timetag s

recvUntil :: Char -> Socket -> IO BS.ByteString
recvUntil term s = go BS.empty
  where
    go accum = do
        c <- liftIO $ recv s 1
        if c == BS.singleton term
          then return accum
          else go (accum <> c)

command :: BS.ByteString -> Timetag -> EitherT String IO (Maybe BS.ByteString)
command cmd (Timetag s) = do
    liftIO $ void $ send s cmd
    reply <- liftIO $ recvUntil '\n' s
    case () of
      _ | BS.null reply                 -> left "timetag_acquire connection terminated"
      _ | "= " `BS.isPrefixOf` reply    -> return $ Just $ BS.drop 2 reply
      _ | "ready" `BS.isPrefixOf` reply -> return Nothing
      _ | "error" `BS.isPrefixOf` reply -> left $ "error reply: "++show reply
      _ | otherwise                     -> left $ "unknown reply: "++show reply

startCapture :: Timetag -> EitherT String IO ()
startCapture = void . command "start_capture\n"

stopCapture :: Timetag -> EitherT String IO ()
stopCapture = void . command "stop_capture\n"

resetCounter :: Timetag -> EitherT String IO ()
resetCounter = void . command "reset_counter\n"

newtype OutputName = OutputName BS.ByteString

addOutputFd :: Timetag -> OutputName -> Fd -> EitherT String IO ()
addOutputFd tt@(Timetag s) (OutputName name) (Fd fd) = do
    command ("add_output_fd "<>name<>"\n") tt
    liftIO $ sendFd s fd

removeOutput :: Timetag -> OutputName -> EitherT String IO ()
removeOutput tt (OutputName name) = do
    void $ command ("remove_output "<>name<>"\n") tt

isCaptureRunning :: Timetag -> EitherT String IO Bool
isCaptureRunning tt = do
    reply <- command "capture?\n" tt
    case reply of
      Just s
        | s == "1" -> return True
        | s == "0" -> return False
      Nothing      -> left "expected reply"
