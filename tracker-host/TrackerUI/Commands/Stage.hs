{-# LANGUAGE RankNTypes #-}

module TrackerUI.Commands.Stage
    ( stageCmds
    , stageSettings
    ) where

import Control.Lens hiding (Setting, setting)
import Linear

import qualified Tracker as T
import Tracker.Types
import TrackerUI.Commands.Utils
import TrackerUI.Types

setRawPositionCmd :: Command
setRawPositionCmd = command ["set-pos"] help "(X,Y,Z)" $ \args->do
    pos <- tryHead "expected position" args >>= tryRead "invalid position"
    liftTrackerE $ T.setRawPosition $ pos^.from (stageV3 . v3Tuple)
  where help = "Set raw stage position"
  
centerCmd :: Command
centerCmd = command ["center"] help "" $ \_->center
  where help = "Set stage at center position"

stageCmds :: [Command]
stageCmds = [setRawPositionCmd, centerCmd]

stageSettings :: [Setting]
stageSettings = concat
    [ r3Setting "stage.output-gain.prop" "stage output proportional gain"
            (knobA T.outputGain) (column propGain . stageV3 . mapping realDouble)
    , r3Setting "stage.output-gain.int" "stage output proportional gain"
            (knobA T.outputGain) (column intGain . stageV3 . mapping realDouble)
    , r3Setting "stage.tau" "stage feedback integration time"
            (knobA T.outputTau) stageV3
    , r3Setting "stage.fb-gain.x" "stage feedback gain"
            (knobA T.stageGain) (_x . stageV3 . mapping realDouble)
    , r3Setting "stage.fb-gain.y" "stage feedback gain"
            (knobA T.stageGain) (_y . stageV3 . mapping realDouble)
    , r3Setting "stage.fb-gain.z" "stage feedback gain"
            (knobA T.stageGain) (_z . stageV3 . mapping realDouble)
    , r3Setting "stage.setpoint" "stage feedback setpoint"
            (knobA T.stageSetpoint) stageV3
    , [setting "stage.max-error"
               "maximum tolerable error signal before killing feedback"
               (knobA T.maxError) id]
    ]
