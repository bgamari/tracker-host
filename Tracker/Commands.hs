{-# LANGUAGE RecordWildCards, DeriveFoldable, DeriveFunctor, DeriveTraversable #-}

module Tracker.Commands where

import Prelude hiding (mapM_)
import Data.Binary
import Data.Binary.Put
import Data.Word
import Data.Int
import Data.Foldable
import Data.Traversable
import qualified Data.ByteString as BS       
import Data.ByteString (ByteString)       
import Tracker.LowLevel
import Linear

echo :: Tracker -> ByteString -> IO (Maybe ByteString)
echo tracker payload = do
    writeCommand tracker 0x0 $ do putByteString payload
    readReply tracker

data Stage a = Stage !a !a !a
             deriving (Show, Functor, Foldable, Traversable)

setStageGains :: Tracker -> Stage (Stage Int32) -> IO ()
setStageGains tracker gains = do
    writeCommand tracker 0x10 $ mapM_ (mapM_ put) gains
    readAck tracker

setStageSetpoint :: Tracker -> Stage Int32 -> IO ()
setStageSetpoint tracker setpoint = do
    writeCommand tracker 0x11 $ mapM_ put setpoint
    readAck tracker

data Psd a = Psd !a !a !a !a
             deriving (Show, Functor, Foldable, Traversable)

setPsdGains :: Tracker -> Psd (Stage Int32) -> IO ()
setPsdGains tracker gains = do
    writeCommand tracker 0x12 $ mapM_ (mapM_ put) gains
    readAck tracker

setPsdSetpoint :: Tracker -> Psd Int32 -> IO ()
setPsdSetpoint tracker setpoint = do
    writeCommand tracker 0x13 $ mapM_ put setpoint
    readAck tracker

setMaxError :: Tracker -> Word32 -> IO ()
setMaxError tracker maxError = do
    writeCommand tracker 0x14 $ put maxError
    readAck tracker

setOutputGains :: Tracker -> Stage Int32 -> IO ()
setOutputGains tracker gains = do
    writeCommand tracker 0x13 $ mapM_ put gains
    readAck tracker
    
data RasterScan = RasterScan { scanCenter :: V2 Word16
                             , scanStep   :: V2 Word16
                             , scanSize   :: V2 Word16
                             , scanFreq   :: Word16
                             }
                deriving (Show)
                
rasterScan :: Tracker -> RasterScan -> IO (Maybe ByteString)
rasterScan tracker (RasterScan {..}) = do
    writeCommand tracker 0x2 $ do
        mapM_ putWord16be scanCenter
        mapM_ putWord16be scanStep
        mapM_ putWord16be scanSize
        putWord16be scanFreq
    readReply tracker
    
setAdcFreq :: Tracker -> Word32 -> IO ()
setAdcFreq tracker freq = do
    writeCommand tracker 0x20 $ put freq
    readAck tracker

startAdcStream :: Tracker -> IO ()
startAdcStream tracker = do
    writeCommand tracker 0x21 $ return ()
    readAck tracker

stopAdcStream :: Tracker -> IO ()
stopAdcStream tracker = do
    writeCommand tracker 0x22 $ return ()
    readAck tracker

setFeedbackFreq :: Tracker -> Word32 -> IO ()
setFeedbackFreq tracker freq = do
    writeCommand tracker 0x30 $ put freq
    readAck tracker

data FeedbackMode = NoFeedback
                  | PsdFeedback
                  | StageFeedback
                  deriving (Show, Eq, Ord, Bounded, Enum)
                  
setFeedbackMode :: Tracker -> FeedbackMode -> IO ()
setFeedbackMode tracker mode = do
    writeCommand tracker 0x30 $ put (fromEnum mode)
    readAck tracker
