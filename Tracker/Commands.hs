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
import qualified Data.Vector as V       
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
    readAck tracker "setStageGains"

setStageSetpoint :: Tracker -> Stage Int32 -> IO ()
setStageSetpoint tracker setpoint = do
    writeCommand tracker 0x11 $ mapM_ putInt32le setpoint
    readAck tracker "setStageSetpoint"

setPsdGains :: Tracker -> Psd (Stage Int32) -> IO ()
setPsdGains tracker gains = do
    writeCommand tracker 0x12 $ mapM_ (mapM_ putInt32le) gains
    readAck tracker "setPsdGains"

setPsdSetpoint :: Tracker -> Psd Int32 -> IO ()
setPsdSetpoint tracker setpoint = do
    writeCommand tracker 0x13 $ mapM_ putInt32le setpoint
    readAck tracker "setPsdSetpoint"

setMaxError :: Tracker -> Word32 -> IO ()
setMaxError tracker maxError = do
    writeCommand tracker 0x14 $ putWord32le maxError
    readAck tracker "setMaxError"

setOutputGains :: Tracker -> Stage Int32 -> IO ()
setOutputGains tracker gains = do
    writeCommand tracker 0x13 $ mapM_ putInt32le gains
    readAck tracker "setOutputGains"

setAdcFreq :: Tracker -> Word32 -> IO ()
setAdcFreq tracker freq = do
    writeCommand tracker 0x20 $ putWord32le freq
    readAck tracker "setAdcFreq"

data TriggerMode = TriggerOff
                 | TriggerAuto
                 | TriggerManual
                 deriving (Show, Eq, Bounded, Enum)

setAdcTriggerMode :: Tracker -> TriggerMode -> IO ()
setAdcTriggerMode tracker mode = do
    writeCommand tracker 0x21 $ putWord32le (fromIntegral $ fromEnum mode)
    readAck tracker "setAdcTriggerMode"

startAdcStream :: Tracker -> IO ()
startAdcStream tracker = do
    writeCommand tracker 0x22 $ return ()
    readAck tracker "startAdcStream"

stopAdcStream :: Tracker -> IO ()
stopAdcStream tracker = do
    writeCommand tracker 0x23 $ return ()
    readAck tracker "stopAdcStream"

setFeedbackFreq :: Tracker -> Word32 -> IO ()
setFeedbackFreq tracker freq = do
    writeCommand tracker 0x30 $ putWord32le freq
    readAck tracker "setFeedbackFreq"

data FeedbackMode = NoFeedback
                  | PsdFeedback
                  | StageFeedback
                  deriving (Show, Eq, Ord, Bounded, Enum)

setFeedbackMode :: Tracker -> FeedbackMode -> IO ()
setFeedbackMode tracker mode = do
    writeCommand tracker 0x30 $ putWord32le (fromIntegral $ fromEnum mode)
    readAck tracker "setFeedbackMode"

maxPathPoints = 80 :: Int

clearPath :: Tracker -> IO ()
clearPath tracker = do
    writeCommand tracker 0x40 $ return ()
    readAck tracker "clearPath"

enqueuePoints :: Tracker -> V.Vector (V3 Word16) -> IO Bool
enqueuePoints tracker points 
  | V.length points > maxPathPoints = return False
  | otherwise = do
      writeCommand tracker 0x41 $ do
          putWord8 $ fromIntegral $ V.length points
          mapM_ (mapM_ putWord16le) points
      maybe False (const True) `fmap` readReply tracker

startPath :: Tracker -> Word32 -> IO ()
startPath tracker freq = do
    writeCommand tracker 0x42 $ putWord32le freq
    readAck tracker "startPath"
