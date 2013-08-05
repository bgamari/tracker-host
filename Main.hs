{-# LANGUAGE OverloadedStrings #-}

import Control.Monad
import Control.Applicative
import Data.Binary.Get
import Data.Int       
       
import qualified Data.Vector.Unboxed as V
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
       
import qualified Tracker as T
import Linear
 
roughScan :: T.RasterScan
roughScan = T.RasterScan { T.scanCenter = pure 0x7fff
                         , T.scanStep   = pure 0x10
                         , T.scanSize   = pure 20
                         , T.scanFreq   = 10
                         }
main = do
    Just t <- T.open
    T.echo t "Hello World!" >>= print
    --T.setStageGains $ 
    T.setFeedbackFreq t 1000
    T.setAdcFreq t 100
    T.roughScan t roughScan
    T.close t
