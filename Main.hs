{-# LANGUAGE OverloadedStrings #-}

import qualified Tracker as T
import Control.Monad
import Data.Binary.Get
import Data.Int       
import qualified Data.ByteString.Lazy as BSL

getInt16le :: Get Int16
getInt16le = fromIntegral `fmap` getWord16le

main = do
    Just t <- T.open
    T.echo t "Hello World!" >>= print
    --T.setStageGains $ 
    T.setFeedbackFreq t 1000
    T.setAdcFreq t 100
    T.startAdcStream t
    forever $ do a <- T.readData t
                 print $ runGet (replicateM 128 $ getInt16le) $ BSL.fromStrict a
    T.close t
