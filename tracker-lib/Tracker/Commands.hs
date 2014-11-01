{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PatternGuards #-}

module Tracker.Commands ( -- * Types
                          Knob
                        , setKnob
                        , getKnob
                        , module Tracker.Commands.Types
                          -- * Knobs
                        , stageGain
                        , stageSetpoint
                        , psdGains
                        , psdSetpoint
                        , maxError
                        , outputGain
                        , outputTau
                        , adcFreq
                        , TriggerMode(..)
                        , adcTriggerMode
                        , adcDecimation
                        , FeedbackMode(..)
                        , feedbackMode
                        , searchStep
                        , searchObjGains
                        , searchObjThresh
                        , coarseFbParams
                          -- * Commands
                        , echo
                        , reset
                        , EventCounters(..)
                        , getEventCounters
                        , setExcitation
                        , startAdcStream
                        , stopAdcStream
                        , flushAdcStream
                        , setFeedbackFreq
                        , setRawPosition
                        , clearPath
                        , maxPathPoints
                        , enqueuePoints
                        , isPathRunning
                        , startPath
                        ) where

import Prelude hiding (mapM_, sequence, sequence_, mapM)
import Data.Binary
import Data.Binary.Put
import Data.Binary.Get
import Data.Int
import Data.Traversable
import Data.Foldable
import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import qualified Data.Vector as V       
import Control.Error
import Control.Applicative
import Control.Monad.IO.Class

import Tracker.LowLevel
import Tracker.Types
import Tracker.Commands.Types

data Knob a = Knob { _knobName    :: String 
                   , _knobGetCmd  :: CmdId
                   , _knobDecode  :: Get a
                   , _knobSetCmd  :: CmdId
                   , _knobEncode  :: a -> Put
                   }
 
setKnob :: MonadIO m => Knob a -> a -> EitherT String (TrackerT m) ()
setKnob knob value = do
    writeCommand (_knobSetCmd knob) $ (_knobEncode knob) value
    readAck $ _knobName knob
        
getKnob :: MonadIO m => Knob a -> EitherT String (TrackerT m) a
getKnob knob = do
    writeCommand (_knobGetCmd knob) $ return ()
    r <- parseReply (_knobDecode knob)
    case r of
      Nothing   -> left $ "getKnob: Error fetching value for "++_knobName knob
      Just x    -> return x
                   
echo :: MonadIO m => ByteString -> EitherT String (TrackerT m) (Maybe ByteString)
echo payload = do
    writeCommand 0x0 $ do putWord8 (fromIntegral $ BS.length payload)
                          putByteString payload
    parseReply $ do len <- getWord8
                    getByteString $ fromIntegral len

reset :: MonadIO m => EitherT String (TrackerT m) ()
reset = writeCommand 0x01 $ putWord32le 0xdeadbeef

data EventCounters = EventCounters { evCountFeedback  :: Word32       
                                   , evCountAdcSample :: Word32
                                   }
                   deriving (Show, Read, Eq, Ord)

getEventCounters :: MonadIO m => EitherT String (TrackerT m) EventCounters
getEventCounters = do
    writeCommand 0x02 $ return ()
    r <- parseReply $ EventCounters <$> getWord32le <*> getWord32le
    case r of
      Nothing   -> left "getEventCounters: Error"
      Just x    -> return x

stageGain :: Knob (Stage (Stage Fixed16))
stageGain = Knob "stage-gain" 0x10 getter 0x11 putter
  where getter = mapM sequence $ pure (pure getFixed16le)
        putter = mapM_ (mapM_ putFixed16le)

stageSetpoint :: Knob (Stage Int32)
stageSetpoint = Knob "stage-setpoint" 0x12 getter 0x13 putter
  where getter = sequence $ pure getInt32le
        putter = mapM_ putInt32le

psdGains :: Knob (Psd (SumDiff (Stage Fixed24)))
psdGains = Knob "psd-gains" 0x14 getter 0x15 putter
  where getter = mapM (mapM sequence) $ pure $ pure (pure getFixed24le)
        putter = mapM_ (mapM_ (mapM_ putFixed24le))

psdSetpoint :: Knob (Psd (SumDiff Int32))
psdSetpoint = Knob "psd-setpoint" 0x16 getter 0x17 putter
  where getter = mapM sequence $ pure $ pure getInt32le
        putter = mapM_ (mapM_ putInt32le)

maxError :: Knob Word32
maxError = Knob "max-error" 0x18 getWord32le 0x19 putWord32le

outputGain :: Knob (Stage (PropInt Fixed16))
outputGain = Knob "output-gain" 0x1a getter 0x1b putter
  where getter = traverse sequence $ pure $ pure getFixed16le
        putter = mapM_ (mapM_ putFixed16le)

outputTau :: Knob (Stage Word8)
outputTau = Knob "output-tau" 0x1c getter 0x1d putter
  where getter = sequence $ pure getWord8
        putter = mapM_ putWord8

setExcitation :: MonadIO m
              => StageAxis -> V.Vector Int16 -> EitherT String (TrackerT m) ()
setExcitation ch samples = do
    writeSamples 0 samples
    setExcitation' ch V.empty 0 (fromIntegral $ V.length samples)
  where maxSamplesPerPacket = (512-6-1) `div` 2
        writeSamples :: MonadIO m
                     => Word16 -> V.Vector Int16
                     -> EitherT String (TrackerT m) ()
        writeSamples offset v
          | V.null v  = return ()
          | otherwise = do
              let (s, rest) = V.splitAt maxSamplesPerPacket v
              setExcitation' ch s offset 0
              writeSamples (offset + fromIntegral (V.length s)) rest

setExcitation' :: MonadIO m
               => StageAxis
               -> V.Vector Int16   -- ^ samples
               -> Word16           -- ^ offset
               -> Word16           -- ^ total_length
               -> EitherT String (TrackerT m) ()
setExcitation' ch samples offset totalLength = do
    writeCommand 0x1e $ do
        putEnum putWord8 ch
        putWord16le $ totalLength
        putWord16le $ offset
        putWord8 $ fromIntegral $ V.length samples
        mapM_ (putWord16le . fromIntegral) samples
    readAck "setExcitation'"

adcFreq :: Knob Word32
adcFreq = Knob "adc-freq" 0x20 getter 0x21 putter
  where getter = getWord32le
        putter = putWord32le

data TriggerMode = TriggerOff
                 | TriggerAuto
                 | TriggerManual
                 deriving (Show, Eq, Bounded, Enum)

adcTriggerMode :: Knob TriggerMode
adcTriggerMode = Knob "adc-trigger-mode" 0x22 getter 0x23 putter
  where getter = getEnum getWord32le
        putter = putEnum putWord32le

adcDecimation :: Knob Word32
adcDecimation = Knob "adc-decimation" 0x24 getter 0x25 putter
  where getter = getWord32le
        putter = putWord32le

startAdcStream :: MonadIO m => EitherT String (TrackerT m) ()
startAdcStream = do
    writeCommand 0x2a $ return ()
    readAck "startAdcStream"

stopAdcStream :: MonadIO m => EitherT String (TrackerT m) ()
stopAdcStream = do
    writeCommand 0x2b $ return ()
    readAck "stopAdcStream"
    
flushAdcStream :: MonadIO m => EitherT String (TrackerT m) ()
flushAdcStream = do
    writeCommand 0x2c $ return ()
    readAck "flushAdcStream"
    
setFeedbackFreq :: MonadIO m => Word32 -> EitherT String (TrackerT m) ()
setFeedbackFreq freq = do
    writeCommand 0x30 $ putWord32le freq
    readAck "setFeedbackFreq"

data FeedbackMode = NoFeedback
                  | PsdFeedback
                  | StageFeedback
                  | SearchFeedback
                  | CoarseFeedback
                  deriving (Show, Eq, Ord, Bounded, Enum)

feedbackMode :: Knob FeedbackMode
feedbackMode = Knob "feedback-mode" 0x31 getter 0x32 putter
  where getter = getEnum getWord8
        putter = putEnum putWord32le

setRawPosition :: MonadIO m => Stage Word16 -> EitherT String (TrackerT m) ()
setRawPosition pos = do
    writeCommand 0x33 $ mapM_ putWord16le pos
    readAck "setRawPosition"

maxPathPoints :: Int
maxPathPoints = 80

clearPath :: MonadIO m => EitherT String (TrackerT m) ()
clearPath = do
    writeCommand 0x40 $ return ()
    readAck "clearPath"

-- | Add points to queued path. Returns @(points_added, path_running)@.
enqueuePoints :: MonadIO m
              => V.Vector (Stage Word16) -> EitherT String (TrackerT m) (Bool, Bool)
enqueuePoints points 
  | V.length points > maxPathPoints =
      left "enqueuePoints: Attempted to enqueue too many points at once"
  | otherwise = do
      writeCommand 0x41 $ do
          putWord8 $ fromIntegral $ V.length points
          mapM_ (mapM_ putWord16le) points
      r <- readReply
      case r of 
        Just r' | [b0, b1] <- BS.unpack r' ->
          let queued = b0 /= 0
              running = b1 /= 0
          in return (queued, running)
        _ ->
          left "enqueuePoints: Unexpected response"

isPathRunning :: MonadIO m => EitherT String (TrackerT m) Bool
isPathRunning = snd <$> enqueuePoints V.empty

startPath :: MonadIO m => Word32 -> Bool -> EitherT String (TrackerT m) ()
startPath freq syncAdc = do
    writeCommand 0x42 $ putWord32le freq >> putWord8 (if syncAdc then 1 else 0)
    readAck "startPath"

searchStep :: Knob (Stage Word16)
searchStep = Knob "search-step" 0x43 getter 0x44 putter
  where getter = sequence $ pure getWord16le
        putter = mapM_ putWord16le

searchObjGains :: Knob (PsdChannels Fixed16)
searchObjGains = Knob "search-obj-gains" 0x45 getter 0x46 putter
  where getter = sequence $ pure getFixed16le
        putter = mapM_ putFixed16le

searchObjThresh :: Knob Word16
searchObjThresh = Knob "search-obj-thresh" 0x47 getWord16le 0x48 putWord16le

coarseFbParams :: Knob (PsdChannels CoarseFbChannel)
coarseFbParams = Knob "coarse-fb-params" 0x49 getter 0x4a putter
  where
    getter = PsdChans <$> traverse sequence (pure $ pure get)
    putter x = sequence_ $ fmap put x
