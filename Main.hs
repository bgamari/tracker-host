{-# LANGUAGE OverloadedStrings #-}

import qualified Tracker as T
import Control.Monad
import Data.Binary.Get
import qualified Data.ByteString.Lazy as BSL

main = do
    Just t <- T.open
    T.echo t "Hello World!" >>= print
    --T.setStageGains $ 
    T.setFeedbackFreq t 1
    T.startAdcStream t
    forever $ do a <- T.readData t
                 print $ runGet (replicateM 127 $ getWord16le) $ BSL.fromStrict a
    T.close t
