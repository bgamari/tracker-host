{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Trap.Aotf
    ( Aotf
    , open
    , Channel
    , channel
    , Amplitude
    , setAmplitude
    , Mode (..)
    , setMode
    ) where

import Control.Error
import Control.Monad.IO.Class
import qualified Data.ByteString.Char8 as BS
import System.IO
import System.Hardware.Serialport

newtype Aotf = Aotf Handle

newtype Amplitude = Amp Int
                  deriving (Show, Ord, Eq, Enum)

instance Bounded Amplitude where
    minBound = Amp 0
    maxBound = Amp 1023

newtype Channel = Ch Int
                deriving (Show, Ord, Eq, Enum)

instance Bounded Channel where
    minBound = Ch 0
    maxBound = Ch 7

channel :: Int -> Maybe Channel
channel c
  | c >= minBound && c <= maxBound = Just $ Ch c
  | otherwise                      = Nothing

open :: MonadIO m => FilePath -> EitherT String m Aotf
open dev = fmapLT show $ tryIO $ do
    h <- hOpenSerial dev defaultSerialSettings
    -- Help auto-baudrate detection
    hPutStr h "\n\n\n"
    -- TODO: Verify device
    -- _ <- getStatus (Aotf h)
    return $ Aotf h

write :: MonadIO m => Aotf -> BS.ByteString -> m ()
write (Aotf h) cmd = do
    liftIO $ BS.hPutStr h (cmd `BS.snoc` '\r')

selectCh :: MonadIO m => Aotf -> Channel -> m ()
selectCh aotf (Ch c) = do
    write aotf $ BS.pack $ "ch"++show c

withChannel :: MonadIO m => Aotf -> Channel -> m () -> m ()
withChannel aotf ch action =
    selectCh aotf ch >> action

setAmplitude :: MonadIO m => Aotf -> Channel -> Amplitude -> m ()
setAmplitude aotf ch (Amp a) =
    withChannel aotf ch $ write aotf $ BS.pack $ "am "++show a

data Mode = On
          | Off
          | Modulate
          deriving (Show)

setMode :: MonadIO m => Aotf -> Channel -> Mode -> m ()
setMode aotf ch mode = do
    withChannel aotf ch $ write aotf cmd
  where
    cmd = case mode of
              On       -> "on"
              Off      -> "off"
              Modulate -> "mod"
