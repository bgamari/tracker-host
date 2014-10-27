{-# LANGUAGE OverloadedStrings #-}

module TrackerUI.Commands.Trap
    ( trapCmds
    , trapSettings
    ) where

import Data.Traversable (sequenceA)
import Control.Monad.IO.Class
import Control.Concurrent.STM
import System.Process (callProcess)

import Statistics.Sample (stdDev)
import qualified Data.Vector as V

import Linear
import Control.Lens hiding (Setting, setting)

import Tracker.LowLevel as T
import Tracker.Types as T
import Tracker.Raster
import TrackerUI.Commands.Utils
import TrackerUI.Types
import Trap
import Trap.Aotf as Aotf

ch0 :: Aotf.Channel
ch0 = maybe (error "bad channel") id $ Aotf.channel 0

scan :: RasterScan T.Stage Double
scan = RasterScan
    { _scanCenter = zero
    , _scanSize   = T.mkStage 60000 60000 0
    , _scanPoints = T.mkStage 300 300 1
    }

stdDevFound :: Double -> ParticleFoundCriterion
stdDevFound s v =
    s < stdDev (V.map realToFrac $ V.map (^. (_x . T.sdSum)) v)

startTrapCmd :: Command
startTrapCmd = command ["trap", "start"] "Start trapping" "" $ \_-> do
    aotf <- liftEitherT $ Aotf.open "/dev/ttyUSB.aotf"
    let exciteCh = ch0
    std <- use particleStd
    bleach <- use bleachThresh
    let cfg = TrapC
            { bleached = (< bleach)
            , foundParticle = stdDevFound std
            , binWidth = round (1 * 128e6 :: Double)
            , setExcitation = \on -> Aotf.setMode aotf exciteCh
                                     $ if on then Aotf.On else Aotf.Off
            , setTrap = \on-> liftIO $ callProcess "thorlabs-laser"
                                      [if on then "--power=200" else "--power=0"]
            , searchScan = map (fmap round) $ rasterScan sequenceA scan
            , outputFiles = ["data/run"++show i | i <- [0 ..] :: [Integer]]
            }
    actions <- liftTrackerE $ Trap.start cfg
    trapActions ?= actions

stopTrapCmd :: Command
stopTrapCmd = command ["trap", "stop"] "Stop trapping" "" $ \_-> do
    use trapActions >>= liftIO . maybe (return ()) trapStop
    trapActions .= Nothing

nextTrapCmd :: Command
nextTrapCmd = command ["trap", "next"] "Move to next particle" "" $ \_-> do
    use trapActions >>= liftIO . maybe (return ()) trapNext

trapStatsCmd :: Command
trapStatsCmd = command ["trap", "stats"] "Print PSD statistics" "" $ \_-> do
    v <- liftTracker $ T.getSensorQueue >>= liftIO . atomically . readTChan
    let s = stdDev $ V.map (^. (T.psd . _x . T.sdSum . to realToFrac)) v
    liftIO $ print $ "Sum stdev "++show s

trapCmds :: [Command]
trapCmds = [ startTrapCmd, nextTrapCmd, stopTrapCmd, trapStatsCmd ]

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
