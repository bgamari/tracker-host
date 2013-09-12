{-# LANGUAGE RecordWildCards #-}

module Tracker.Commands ( -- * Types
                          Knob
                        , setKnob
                        , getKnob
                          -- * Knobs
                        , stageGain
                        , stageSetpoint
                        , psdGains
                        , psdSetpoint
                        , maxError
                        , outputGain
                        , FeedbackMode(..)
                        , feedbackMode
                          -- * Commands
                        , echo
                        , reset
                        , setExcitation
                        , setAdcFreq
                        , TriggerMode(..)
                        , setAdcTriggerMode
                        , startAdcStream
                        , stopAdcStream
                        , setFeedbackFreq
                        , setRawPosition
                        , clearPath
                        , maxPathPoints
                        , enqueuePoints
                        , isPathRunning
                        , startPath
                        ) where

import Prelude hiding (mapM_, sequence, mapM)
import Data.Binary
import Data.Binary.Put
import Data.Binary.Get
import Data.Maybe
import Data.Word
import Data.Int
import Data.Traversable
import Data.Foldable
import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import qualified Data.Vector as V       
import Control.Error
import Control.Applicative
import Control.Monad (liftM)
import Control.Monad.IO.Class
import Tracker.LowLevel
import Tracker.Types
import Control.Lens
import Linear

-- | Put a 32-bit signed integer
--
-- Exploits the fact that 'fromIntegral' preserves representation, not sign
putInt32le :: Int32 -> Put
putInt32le = putWord32le . fromIntegral

getInt32le :: Get Int32
getInt32le = fromIntegral <$> getWord32le

data Knob a = Knob { _knobName    :: String 
                   , _knobGetCmd  :: CmdId
                   , _knobDecode  :: Get a
                   , _knobSetCmd  :: CmdId
                   , _knobEncode  :: a -> Put
                   }
 
setKnob :: MonadIO m => Knob a -> a -> TrackerT m ()
setKnob knob value = do
    writeCommand (_knobSetCmd knob) $ (_knobEncode knob) value
    readAck $ _knobName knob
        
getKnob :: MonadIO m => Knob a -> TrackerT m a
getKnob knob = do
    writeCommand (_knobGetCmd knob) $ return ()
    r <- parseReply (_knobDecode knob)
    maybe (error $ _knobName knob) return r
                   
echo :: MonadIO m => ByteString -> TrackerT m (Maybe ByteString)
echo payload = do
    writeCommand 0x0 $ do putWord8 (fromIntegral $ BS.length payload)
                          putByteString payload
    parseReply $ do length <- getWord8
                    getByteString $ fromIntegral length

reset :: MonadIO m => TrackerT m ()
reset = writeCommand 0x01 $ putWord32le 0xdeadbeef

stageGain :: Knob (Stage (Stage Fixed16))
stageGain = Knob "stage-gain" 0x10 getter 0x11 putter
  where getter = mapM sequence $ pure (pure getFixed16le)
        putter = mapM_ (mapM_ putFixed16le)

stageSetpoint :: Knob (Stage Int32)
stageSetpoint = Knob "stage-setpoint" 0x12 getter 0x13 putter
  where getter = sequence $ pure getInt32le
        putter = mapM_ putInt32le

psdGains :: Knob (Psd (Stage Fixed16))
psdGains = Knob "psd-gains" 0x14 getter 0x15 putter
  where getter = mapM sequence $ pure (pure getFixed16le)
        putter = mapM_ (mapM_ putFixed16le)

psdSetpoint :: Knob (Psd Int32)
psdSetpoint = Knob "psd-setpoint" 0x16 getter 0x17 putter
  where getter = sequence $ pure getInt32le
        putter = mapM_ putInt32le

maxError :: Knob Word32
maxError = Knob "max-error" 0x18 getWord32le 0x19 putWord32le

outputGain :: Knob (Stage Fixed16)
outputGain = Knob "output-gain" 0x1a getter 0x1b putter
  where getter = sequence $ pure getFixed16le
        putter = mapM_ putFixed16le

setExcitation :: MonadIO m => StageAxis -> V.Vector Int16 -> TrackerT m ()
setExcitation ch samples = do
    writeCommand 0x16 $ do
      putWord8 $ fromIntegral $ fromEnum ch
      putWord8 $ fromIntegral $ V.length samples
      mapM_ (putWord16le . fromIntegral) samples
    readAck "setExcitation"

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

feedbackMode :: Knob FeedbackMode
feedbackMode = Knob "feedback-mode" 0x31 getter 0x32 putter
  where getter = (toEnum . fromIntegral) `liftM` getWord8
        putter = putWord32le . fromIntegral . fromEnum

setRawPosition :: MonadIO m => Stage Word16 -> TrackerT m ()
setRawPosition pos = do
    writeCommand 0x33 $ mapM_ putWord16le pos
    readAck "setRawPosition"

maxPathPoints = 80 :: Int

clearPath :: MonadIO m => TrackerT m ()
clearPath = do
    writeCommand 0x40 $ return ()
    readAck "clearPath"

enqueuePoints :: MonadIO m => V.Vector (Stage Word16) -> TrackerT m (Maybe Bool)
enqueuePoints points 
  | V.length points > maxPathPoints = return Nothing
  | otherwise = do
      writeCommand 0x41 $ do
          putWord8 $ fromIntegral $ V.length points
          mapM_ (mapM_ putWord16le) points
      r <- readReply
      case r of 
        Nothing                     -> return Nothing
        Just r' | BS.length r' == 1 -> return $ Just $ BS.head r' /= 0
        otherwise                   -> return Nothing

isPathRunning :: MonadIO m => TrackerT m Bool
isPathRunning = do
    s <- enqueuePoints V.empty
    case s of
      Just a  -> return a
      Nothing -> error "isPathRunning: Unexpected nack"

startPath :: MonadIO m => Word32 -> Bool -> TrackerT m ()
startPath freq syncAdc = do
    writeCommand 0x42 $ putWord32le freq >> putWord8 (if syncAdc then 1 else 0)
    readAck "startPath"
