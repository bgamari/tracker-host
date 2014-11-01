{-# LANGUAGE TemplateHaskell #-}

module Tracker.Commands.Types where

import Control.Applicative
import Data.Foldable
import Data.Traversable as T
import Data.Word
import Data.Int

import Data.Binary
import Data.Binary.Put
import Data.Binary.Get

import Control.Lens
import Linear

getEnum :: (Integral n, Enum a) => Get n -> Get a
getEnum getN = toEnum . fromIntegral <$> getN

putEnum :: (Integral n, Enum a) => (n -> Put) -> (a -> Put)
putEnum putN = putN . fromIntegral . fromEnum

-- | Put a 32-bit signed integer
--
-- Exploits the fact that 'fromIntegral' preserves representation, not sign
putInt32le :: Int32 -> Put
putInt32le = putWord32le . fromIntegral

getInt32le :: Get Int32
getInt32le = fromIntegral <$> getWord32le

putInt16le :: Int16 -> Put
putInt16le = putWord16le . fromIntegral

getInt16le :: Get Int16
getInt16le = fromIntegral <$> getWord16le

data CoarseFbChannel
    = CoarseFbChan { _coarseStepHigh  :: V3 Int16
                   , _coarseStepLow   :: V3 Int16
                   , _coarseTolerance :: Word16
                   }
    deriving (Show, Eq, Ord)
    
makeLenses ''CoarseFbChannel

instance Binary CoarseFbChannel where
    get = CoarseFbChan <$> getV3 <*> getV3 <*> getWord16le
      where
        getV3 :: Get (V3 Int16)
        getV3 = T.sequence $ pure getInt16le
    put (CoarseFbChan h l t) = do
        traverse_ putInt16le h
        traverse_ putInt16le l
        putWord16le t
