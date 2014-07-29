{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.Error.Class
import Control.Monad.State
import Control.Monad.Trans.Class
import Control.Monad.Trans.Either
import Control.Monad.IO.Class
import Control.Applicative
import Control.Concurrent

import Control.Monad (MonadPlus(..))
import Control.Error.Util
import Data.EitherR (fmapLT)
import Data.Functor.Rep

import System.IO
import qualified Data.ByteString as BS

import Linear
import System.Console.Haskeline
import Data.Binary.Get
import Control.Lens

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
          liftTrackerE $ do T.echo "Hello World!" >>= liftIO . print
                            T.setKnob T.stageGain defaultStageGains
                            T.setKnob T.outputGain defaultOutputGains
                            T.setFeedbackFreq 50000
                            T.setKnob T.adcFreq 50000
                            T.setKnob T.adcDecimation 4
                            T.startAdcStream
                            T.setKnob T.adcTriggerMode T.TriggerAuto
          while $ prompt

while :: Monad m => m Bool -> m ()
while m = m >>= \a->when a (while m)
