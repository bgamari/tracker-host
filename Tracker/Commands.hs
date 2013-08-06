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
import Control.Monad (liftM)
import Tracker.Monad
import Tracker.LowLevel
import Tracker.Types
import Linear

-- | Put a 32-bit signed integer
--
-- Exploits the fact that 'fromIntegral' preserves representation, not sign
putInt32le :: Int32 -> Put
putInt32le = putWord32le . fromIntegral

echo :: MonadIO m => ByteString -> TrackerT m (Maybe ByteString)
echo payload = do
    writeCommand 0x0 $ do putWord8 (fromIntegral $ BS.length payload)
                          putByteString payload
    parseReply $ do length <- getWord8
                    getByteString $ fromIntegral length

setStageGains :: MonadIO m => Stage (Stage Int32) -> TrackerT m ()
setStageGains gains = do
    writeCommand 0x10 $ mapM_ (mapM_ putInt32le) gains
    readAck "setStageGains"

setStageSetpoint :: MonadIO m => Stage Int32 -> TrackerT m ()
setStageSetpoint setpoint = do
    writeCommand 0x11 $ mapM_ putInt32le setpoint
    readAck "setStageSetpoint"

setPsdGains :: MonadIO m => Psd (Stage Int32) -> TrackerT m ()
setPsdGains gains = do
    writeCommand 0x12 $ mapM_ (mapM_ putInt32le) gains
    readAck "setPsdGains"

setPsdSetpoint :: MonadIO m => Psd Int32 -> TrackerT m ()
setPsdSetpoint setpoint = do
    writeCommand 0x13 $ mapM_ putInt32le setpoint
    readAck "setPsdSetpoint"

setMaxError :: MonadIO m => Word32 -> TrackerT m ()
setMaxError maxError = do
    writeCommand 0x14 $ putWord32le maxError
    readAck "setMaxError"

setOutputGains :: MonadIO m => Stage Int32 -> TrackerT m ()
setOutputGains gains = do
    writeCommand 0x13 $ mapM_ putInt32le gains
    readAck "setOutputGains"

setAdcFreq :: MonadIO m => Word32 -> TrackerT m ()
setAdcFreq freq = do
    writeCommand 0x20 $ putWord32le freq
    readAck "setAdcFreq"

data TriggerMode = TriggerOff
                 | TriggerAuto
                 | TriggerManual
                 deriving (Show, Eq, Bounded, Enum)

setAdcTriggerMode :: MonadIO m => TriggerMode -> TrackerT m ()
setAdcTriggerMode mode = do
    writeCommand 0x21 $ putWord32le (fromIntegral $ fromEnum mode)
    readAck "setAdcTriggerMode"

startAdcStream :: MonadIO m => TrackerT m ()
startAdcStream = do
    writeCommand 0x22 $ return ()
    readAck "startAdcStream"

stopAdcStream :: MonadIO m => TrackerT m ()
stopAdcStream = do
    writeCommand 0x23 $ return ()
    readAck "stopAdcStream"

setFeedbackFreq :: MonadIO m => Word32 -> TrackerT m ()
setFeedbackFreq freq = do
    writeCommand 0x30 $ putWord32le freq
    readAck "setFeedbackFreq"

data FeedbackMode = NoFeedback
                  | PsdFeedback
                  | StageFeedback
                  deriving (Show, Eq, Ord, Bounded, Enum)

setFeedbackMode :: MonadIO m => FeedbackMode -> TrackerT m ()
setFeedbackMode mode = do
    writeCommand 0x30 $ putWord32le (fromIntegral $ fromEnum mode)
    readAck "setFeedbackMode"

maxPathPoints = 80 :: Int

clearPath :: MonadIO m => TrackerT m ()
clearPath = do
    writeCommand 0x40 $ return ()
    readAck "clearPath"

enqueuePoints :: MonadIO m => V.Vector (V3 Word16) -> TrackerT m Bool
enqueuePoints points 
  | V.length points > maxPathPoints = return False
  | otherwise = do
      writeCommand 0x41 $ do
          putWord8 $ fromIntegral $ V.length points
          mapM_ (mapM_ putWord16le) points
      maybe False (const True) `liftM` readReply

startPath :: MonadIO m => Word32 -> TrackerT m ()
startPath freq = do
    writeCommand 0x42 $ putWord32le freq
    readAck "startPath"
