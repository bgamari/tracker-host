{-# LANGUAGE RankNTypes #-}

module TrackerUI.Commands.Feedback
    ( feedbackCmds
    , feedbackSettings
    ) where

import Prelude hiding (concat)
import Data.Monoid ((<>))
import Data.Foldable
import Control.Monad.Trans.Class

import qualified Data.Vector as V

import Control.Concurrent.STM

import Control.Lens hiding (Setting, setting)
import Linear

import System.Console.Haskeline

import qualified Tracker as T
import Tracker.Types
import TrackerUI.Commands.Utils
import TrackerUI.Types

psdChannelNames :: T.PsdChannels String
psdChannelNames =
    PsdChans $ T.mkPsd (T.mkSumDiff "x.sum" "x.diff")
                       (T.mkSumDiff "y.sum" "y.diff")

tabulatePsdChannels :: ((forall a. Lens' (T.PsdChannels a) a) -> b)
                    -> T.PsdChannels b
tabulatePsdChannels f = T.PsdChans $ T.mkPsd
    (T.mkSumDiff (f (_Wrapped' . _x . T.sdSum))
                 (f (_Wrapped' . _x . T.sdDiff)))
    (T.mkSumDiff (f (_Wrapped' . _y . T.sdSum))
                 (f (_Wrapped' . _y . T.sdDiff)))

setPsdSetpointCmd :: Command
setPsdSetpointCmd = command ["set-psd-setpoint"] help "" $ \_->do
    liftTrackerE $ do
      s <- lift T.getSensorQueue >>= liftIO . atomically . readTChan
      let accum :: T.PsdChannels Integer
          accum = V.foldl (\acc x->acc ^+^ fmap fromIntegral (x ^. T.psd . _Unwrapped')) zero s
          n = fromIntegral $ V.length s
      T.setKnob T.psdSetpoint $ (accum & mapped %~ \x->fromIntegral $ x `div` n) ^. _Wrapped'
  where help = "Set PSD feedback setpoint to current sensor values"

feedbackCmds :: [Command]
feedbackCmds =
    [ command ["feedback", "stop"] "Stop feedback" "" $ \_->
        liftTrackerE $ T.setKnob T.feedbackMode T.NoFeedback
    , command ["feedback", "psd"] "Start PSD feedback" "" $ \_->
        liftTrackerE $ T.setKnob T.feedbackMode T.PsdFeedback
    , command ["feedback", "stage"] "Start stage feedback" "" $ \_->
        liftTrackerE $ T.setKnob T.feedbackMode T.StageFeedback
    , command ["feedback", "search"] "Start particle search feedback" "" $ \_->
        liftTrackerE $ T.setKnob T.feedbackMode T.SearchFeedback
    , command ["feedback", "coarse"] "Start coarse feedback" "" $ \_->
        liftTrackerE $ T.setKnob T.feedbackMode T.CoarseFeedback
    , command ["feedback", "status"] "Show feedback status" "" $ \_->
        liftTrackerE (T.getKnob T.feedbackMode) >>= liftInputT . outputStrLn . show
    , setPsdSetpointCmd
    ]

psdSettings :: [Setting]
psdSettings = concat $ toList $ tabulatePsdChannels channel
  where
    channel :: (forall a. Lens' (PsdChannels a) a) -> [Setting]
    channel l = concat
        [ r3Setting ("psd.fb-gain."<>name) "PSD feedback gain"
                    (knobA T.psdGains)
                    (_Unwrapped' . l . stageV3 . mapping realDouble)
        , [setting ("psd.fb-setpoint."<>name)
                   "PSD feedback setpoint"
                   (knobA T.psdSetpoint) (_Unwrapped' . l)]
        ]
      where
        name = psdChannelNames ^. l

searchSettings :: [Setting]
searchSettings = concat
    [ r3Setting "search.step" "Search feedback step size"
                (knobA T.searchStep) stageV3
    , [setting "search.obj-thresh"
               "Search objective function threshold"
               (knobA T.searchObjThresh) id]
    , toList $ tabulatePsdChannels gain
    ]
  where
    gain :: (forall a. Lens' (PsdChannels a) a) -> Setting
    gain l =
        setting ("search.gains."<>name) "Search objective gain"
                (knobA T.searchObjGains) l
      where
        name = psdChannelNames ^. l

coarseFbSettings :: [Setting]
coarseFbSettings = concat $ toList $ tabulatePsdChannels go
  where
    go :: (forall a. Lens' (PsdChannels a) a) -> [Setting]
    go l = concat
        [ r3Setting ("coarse."<>name<>".high.step")
                    "Coarse feedback high step"
                    (knobA T.coarseFbParams)
                    (l . T.coarseStepHigh)
        , r3Setting ("coarse."<>name<>".low.step")
                    "Coarse feedback low step"
                    (knobA T.coarseFbParams)
                    (l . T.coarseStepLow)
        , [setting ("coarse."<>name<>".tol")
                   "Coarse feedback low step"
                   (knobA T.coarseFbParams)
                   (l . T.coarseTolerance)]
        ]
      where
        name = psdChannelNames ^. l

feedbackSettings :: [Setting]
feedbackSettings =
    psdSettings ++ searchSettings ++ coarseFbSettings ++
    [ setting "feedback.freq" "Feedback loop update frequency"
              (knobA T.feedbackFreq) id
    ]
