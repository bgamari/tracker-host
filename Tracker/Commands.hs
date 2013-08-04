{-# LANGUAGE RecordWildCards #-}

module Tracker.Commands where

import Prelude hiding (mapM_)
import Data.Binary
import Data.Binary.Put
import Data.Word
import Data.Foldable
import qualified Data.ByteString as BS       
import Data.ByteString (ByteString)       
import Tracker.LowLevel
import Linear

echo :: Tracker -> ByteString -> IO (Maybe ByteString)
echo tracker payload = do
    writeCommand tracker 0x0 $ do putByteString payload
    readReply tracker
    
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
