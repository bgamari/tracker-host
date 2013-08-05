{-# LANGUAGE OverloadedStrings #-}

import qualified Tracker as T
import Control.Monad
import Data.Binary.Get
import Data.Int       
       
import qualified Data.Vector.Unboxed as V
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL

getInt16le :: Get Int16
getInt16le = fromIntegral `fmap` getWord16le

parseSamples :: BS.ByteString -> V.Vector Int16
parseSamples a = 
    runGet (V.replicateM (BS.length a `div` 2) $ getInt16le) $ BSL.fromStrict a
    
main = do
    Just t <- T.open
    T.echo t "Hello World!" >>= print
    --T.setStageGains $ 
    T.setFeedbackFreq t 1000
    T.setAdcFreq t 100
    T.startAdcStream t
    forever $ do samples <- parseSamples `fmap` T.readData t
                 print samples
    T.close t
