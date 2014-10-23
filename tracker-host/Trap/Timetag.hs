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
      -- * Managing capture outputs
    , OutputId
    , addOutputFd
    , removeOutput
    ) where

import Control.Error
import Control.Monad (void, when)
import Control.Monad.IO.Class
import Data.Monoid
import qualified Data.ByteString.Char8 as BS

import Network.Socket hiding (send, sendTo, recv, recvFrom, close)
import qualified Network.Socket as Socket
import Network.Socket.ByteString
import System.Posix.Types

newtype Timetag = Timetag Socket

tryIO' :: MonadIO m => IO a -> EitherT String m a
tryIO' = fmapLT show . tryIO

open :: MonadIO m => FilePath -> EitherT String m Timetag
open path = do
    s <- tryIO' $ socket AF_UNIX Stream defaultProtocol
    tryIO' $ connect s (SockAddrUnix path)
    ready <- tryIO' $ recvUntil '\n' s
    if ready `BS.isPrefixOf` ready
      then return $ Timetag s
      else left $ "Expected 'ready', saw "++show ready

close :: MonadIO m => Timetag -> EitherT String m ()
close (Timetag s) = tryIO' $ Socket.close s

recvUntil :: Char -> Socket -> IO BS.ByteString
recvUntil term s = go BS.empty
  where
    go !accum = do
        c <- liftIO $ recv s 1
        case () of
          () | BS.null c         -> return accum
             | BS.head c == term -> return accum
             | otherwise         -> go (accum <> c)

command :: BS.ByteString -> Timetag -> EitherT String IO (Maybe BS.ByteString)
command cmd tt@(Timetag s) = do
    n <- tryIO' $ send s cmd
    readReply tt

readReply :: Timetag -> EitherT String IO (Maybe BS.ByteString)
readReply (Timetag s) = go BS.empty
  where
    go !accum = do
      reply <- tryIO' $ recvUntil '\n' s
      case () of
        _ | BS.null reply                 ->
            left "Timetag.readReply: timetag_acquire connection terminated"
        _ | "= " `BS.isPrefixOf` reply    -> do
             go $ accum <> BS.drop 2 reply
        _ | "ready" `BS.isPrefixOf` reply -> do
             return $ if BS.null accum then Nothing else Just accum
        _ | "error" `BS.isPrefixOf` reply -> do
            ready <- tryIO' $ recvUntil '\n' s -- kill next "ready"
            when (not $ "ready" `BS.isPrefixOf` ready)
                $ left $ "Expected 'ready' after 'error', saw "++show ready
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
    let cmd = "add_output_fd "<>name<>"\n"
    tryIO' $ send s cmd
    _ <- readReply tt
    tryIO' $ sendFd s fd
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
        | otherwise-> left $ "invalid reply "++show s
      Nothing      -> left "expected reply"
