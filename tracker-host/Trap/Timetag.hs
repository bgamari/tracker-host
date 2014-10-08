{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}

module Trap.Timetag
    ( Timetag
    , open
    , startCapture
    , stopCapture
    , resetCounter
    , isCaptureRunning
    , OutputId
    , addOutputFd
    , removeOutput
    ) where

import Control.Error
import Control.Monad (void)
import Control.Monad.IO.Class
import Data.Monoid
import qualified Data.ByteString.Char8 as BS
import Control.Concurrent (threadDelay)

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
command cmd tt@(Timetag s) = do
    liftIO $ send s cmd
    readReply tt

readReply :: Timetag -> EitherT String IO (Maybe BS.ByteString)
readReply (Timetag s) = do
    reply <- liftIO $ recvUntil '\n' s
    case () of
      _ | BS.null reply                 ->
          left "Timetag.readReply: timetag_acquire connection terminated"
      _ | "= " `BS.isPrefixOf` reply    -> return $ Just $ BS.drop 2 reply
      _ | "ready" `BS.isPrefixOf` reply -> return Nothing
      _ | "error" `BS.isPrefixOf` reply ->
          left $ "Timetag.readReply: error reply: "++show reply
      _ | otherwise                     ->
          left $ "Timetag.readReply: unknown reply: "++show reply

startCapture :: Timetag -> EitherT String IO ()
startCapture = void . command "start_capture\n"

stopCapture :: Timetag -> EitherT String IO ()
stopCapture = void . command "stop_capture\n"

resetCounter :: Timetag -> EitherT String IO ()
resetCounter = void . command "reset_counter\n"

newtype OutputId = OutputId Int deriving (Show, Eq, Ord)

addOutputFd :: Timetag
            -> BS.ByteString  -- ^ Friendly name
            -> Fd             -- ^ The fd to recieve the output
            -> EitherT String IO OutputId
addOutputFd tt@(Timetag s) name (Fd fd) = do
    liftIO $ send s ("add_output_fd "<>name<>"\n")
    liftIO $ threadDelay 10000
    liftIO $ sendFd s fd
    reply <- readReply tt
    case reply of
      Just reply'
        | [(outputId,_)] <- reads $ BS.unpack reply' ->
          return $ OutputId outputId
        | otherwise ->
          left "Timetag.addOutputFd: Couldn't parse reply"
      Nothing        ->
          left "Timetag.addOutputFd: Expected reply"

removeOutput :: Timetag -> OutputId -> EitherT String IO ()
removeOutput tt (OutputId outputId) = do
    void $ command ("remove_output "<>BS.pack (show outputId)<>"\n") tt

isCaptureRunning :: Timetag -> EitherT String IO Bool
isCaptureRunning tt = do
    reply <- command "capture?\n" tt
    case reply of
      Just s
        | s == "1" -> return True
        | s == "0" -> return False
      Nothing      -> left "expected reply"
