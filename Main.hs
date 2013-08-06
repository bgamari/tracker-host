{-# LANGUAGE OverloadedStrings #-}

import Control.Monad
import Control.Applicative
import Data.Binary.Get
import Data.Int       
       
import qualified Data.Vector as V
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
       
import qualified Tracker as T
import Linear
 
roughScan :: T.RasterScan
roughScan =
    maybe (error "Invalid scan") id
    $ T.scanAround (pure 0x7fff) (pure 0x1000) (V3 20 20 2)

main = do
    Just t <- T.open
    T.echo t "Hello World!" >>= print
    --T.setStageGains $ 
    T.setFeedbackFreq t 1000
    T.setAdcFreq t 100
    T.roughScan t 1000 roughScan >>= V.mapM_ print
    T.close t
