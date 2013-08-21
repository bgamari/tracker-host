module PreAmp ( PreAmp
              , open
              , Channel
              , channels
              , setOffset
              , setGain
              ) where

import System.Hardware.Serialport
import System.IO
import Tracker.Types
import Linear
import Control.Applicative

newtype PreAmp = PreAmp Handle

newtype Channel = Ch Int

channels = Psd $ V2 (SumDiff xSum xDiff) (SumDiff ySum yDiff)

xSum  = Ch 0
xDiff = Ch 1
ySum  = Ch 2
yDiff = Ch 3

setOffset :: PreAmp -> Channel -> Int -> IO ()
setOffset (PreAmp h) (Ch n) v = do
    hPutStr h $ show n++"o="++show v++"\r\n"
    hGetLine h >>= print

setGain :: PreAmp -> Channel -> Int -> IO ()
setGain (PreAmp h) (Ch n) v = do
    hPutStr h $ show n++"g="++show v++"\r\n"
    hGetLine h >>= print

open :: FilePath -> IO PreAmp
open port = do
    PreAmp <$> hOpenSerial port defaultSerialSettings { commSpeed = CS115200 }
