{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.State
import Control.Applicative

import Linear
import System.Console.Haskeline

import qualified Tracker as T
import Tracker.Types
import TrackerUI.Types
import TrackerUI.Commands

prompt :: TrackerUI Bool
prompt = do
    input <- maybe ["exit"] words <$> liftInputT (getInputLine "> ")
    runCommand input

defaultStageGains :: Stage (Stage Fixed16)
defaultStageGains = kronecker $ Stage $ V3 1 1 1

defaultOutputGains :: Stage (PropInt Fixed16)
defaultOutputGains = pure (PropInt 1e-2 0)

main :: IO ()
main = either error (const $ return ()) =<< go
  where go = runTrackerUI commands $ do
          liftTrackerE $ do
              T.echo "Hello World!" >>= liftIO . print
              T.setKnob T.stageGain defaultStageGains
              T.setKnob T.outputGain defaultOutputGains
              T.setFeedbackFreq 50000
              T.setKnob T.adcFreq 50000
              T.setKnob T.adcDecimation 4
              T.startAdcStream
              T.setKnob T.adcTriggerMode T.TriggerAuto
          while $ prompt
          liftTrackerE $ T.stopAdcStream

while :: Monad m => m Bool -> m ()
while m = m >>= \a->when a (while m)
