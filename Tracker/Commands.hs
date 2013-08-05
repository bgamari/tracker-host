{-# LANGUAGE RecordWildCards #-}

module Tracker.Commands where

import Prelude hiding (mapM_)
import Data.Binary
import Data.Binary.Put
import Data.Binary.Get
import Data.Word
import Data.Int
import Data.Traversable
import Data.Foldable
import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import Tracker.LowLevel
import Tracker.Types
import Linear

-- | Put a 32-bit signed integer
--
-- Exploits the fact that 'fromIntegral' preserves representation, not sign
putInt32le :: Int32 -> Put
putInt32le = putWord32le . fromIntegral

echo :: Tracker -> ByteString -> IO (Maybe ByteString)
echo tracker payload = do
    writeCommand tracker 0x0 $ do putWord8 (fromIntegral $ BS.length payload)
                                  putByteString payload
    parseReply tracker $ do length <- getWord8
                            getByteString $ fromIntegral length

setStageGains :: Tracker -> Stage (Stage Int32) -> IO ()
setStageGains tracker gains = do
    writeCommand tracker 0x10 $ mapM_ (mapM_ putInt32le) gains
    readAck tracker

setStageSetpoint :: Tracker -> Stage Int32 -> IO ()
setStageSetpoint tracker setpoint = do
    writeCommand tracker 0x11 $ mapM_ putInt32le setpoint
    readAck tracker

setPsdGains :: Tracker -> Psd (Stage Int32) -> IO ()
setPsdGains tracker gains = do
    writeCommand tracker 0x12 $ mapM_ (mapM_ putInt32le) gains
    readAck tracker

setPsdSetpoint :: Tracker -> Psd Int32 -> IO ()
setPsdSetpoint tracker setpoint = do
    writeCommand tracker 0x13 $ mapM_ putInt32le setpoint
    readAck tracker

setMaxError :: Tracker -> Word32 -> IO ()
setMaxError tracker maxError = do
    writeCommand tracker 0x14 $ putWord32le maxError
    readAck tracker

setOutputGains :: Tracker -> Stage Int32 -> IO ()
setOutputGains tracker gains = do
    writeCommand tracker 0x13 $ mapM_ putInt32le gains
    readAck tracker

data RasterScan = RasterScan { scanCenter :: V2 Word16 -- in codepoints
                             , scanStep   :: V2 Word16 -- in codepoints
                             , scanSize   :: V2 Word16 -- points
                             , scanFreq   :: Word16    -- in Hz
                             }
                deriving (Show)

rasterScan :: Tracker -> RasterScan -> IO (Maybe ByteString)
rasterScan tracker (RasterScan {..}) = do
    writeCommand tracker 0x2 $ do
        mapM_ putWord16le scanCenter
        mapM_ putWord16le scanStep
        mapM_ putWord16le scanSize
        putWord16le scanFreq
    readReply tracker

setAdcFreq :: Tracker -> Word32 -> IO ()
setAdcFreq tracker freq = do
    writeCommand tracker 0x20 $ putWord32le freq
    readAck tracker

data TriggerMode = TriggerOff
                 | TriggerAuto
                 | TriggerManual
                 deriving (Show, Eq, Bounded, Enum)

setAdcTriggerMode :: Tracker -> TriggerMode -> IO ()
setAdcTriggerMode tracker mode = do
    writeCommand tracker 0x21 $ putWord32le (fromIntegral $ fromEnum mode)
    readAck tracker

startAdcStream :: Tracker -> IO ()
startAdcStream tracker = do
    writeCommand tracker 0x22 $ return ()
    readAck tracker

stopAdcStream :: Tracker -> IO ()
stopAdcStream tracker = do
    writeCommand tracker 0x23 $ return ()
    readAck tracker

setFeedbackFreq :: Tracker -> Word32 -> IO ()
setFeedbackFreq tracker freq = do
    writeCommand tracker 0x30 $ putWord32le freq
    readAck tracker

data FeedbackMode = NoFeedback
                  | PsdFeedback
                  | StageFeedback
                  deriving (Show, Eq, Ord, Bounded, Enum)

setFeedbackMode :: Tracker -> FeedbackMode -> IO ()
setFeedbackMode tracker mode = do
    writeCommand tracker 0x30 $ putWord32le (fromIntegral $ fromEnum mode)
    readAck tracker
