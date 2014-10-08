{-# LANGUAGE OverloadedStrings #-}

module TrackerUI.Commands.Trap
    ( trapCmds
    , trapSettings
    ) where

import Data.Traversable (sequenceA)

import Control.Monad.IO.Class
import Control.Monad.Trans.Either
import Control.Concurrent.Async

import Linear
import Control.Lens hiding (Setting, setting)

import System.Process (callProcess)

import Tracker.Types as T
import Tracker.Raster
import TrackerUI.Commands.Utils
import TrackerUI.Types
import Tracker.LowLevel (liftThrough)       
import Trap
import Trap.Aotf as Aotf

ch0 :: Aotf.Channel
ch0 = maybe (error "bad channel") id $ Aotf.channel 0

scan :: RasterScan T.Stage Double
scan = RasterScan
    { _scanCenter = zero
    , _scanSize   = T.mkStage 60000 60000 0
    , _scanPoints = T.mkStage 30 30 1
    }

startTrapCmd :: Command
startTrapCmd = command ["trap", "start"] "Start trapping" "" $ \_-> do
    aotf <- liftEitherT $ Aotf.open "/dev/ttyUSB.aotf"
    let exciteCh = ch0
    std <- use particleStd
    bleach <- use bleachThresh
    let cfg = TrapC
            { bleached = (> bleach)
            , foundParticle = stdDevFound std
            , binWidth = round (1 * 128e6 :: Double)
            , setExcitation = \on -> Aotf.setMode aotf exciteCh
                                     $ if on then Aotf.On else Aotf.Off
            , setTrap = \on-> liftIO $ callProcess "thorlabs-laser"
                                      [if on then "--on" else "--off"]
            , searchScan = map (fmap round) $ rasterScan sequenceA scan
            }
    thread <- liftTracker $ liftThrough async $ runEitherT $ Trap.start cfg
    stopTrap ?= liftIO (cancel thread)

stopTrapCmd :: Command
stopTrapCmd = command ["trap", "stop"] "Stop trapping" "" $ \_-> do
    use stopTrap >>= maybe (return ()) id

trapCmds :: [Command]
trapCmds = [ startTrapCmd, stopTrapCmd ]

trapSettings :: [Setting]         
trapSettings =
    [ setting "trap.bleach-thresh"
              "Fluorescence intensity threshold for trap termination"
              stateA
              bleachThresh
    , setting "trap.particle-std"
              "Threshold on standard deviation of sum signal to determine when a particle is present"
              stateA
              particleStd
    ]
