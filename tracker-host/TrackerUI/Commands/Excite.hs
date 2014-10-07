{-# LANGUAGE RankNTypes #-}

module TrackerUI.Commands.Excite (exciteCmds, exciteSettings) where

import Control.Applicative
import System.IO
import Control.Monad.IO.Class

import Control.Lens hiding (Setting, setting)
import Linear

import Control.Concurrent.STM

import qualified Data.Vector as V

import qualified Tracker as T
import TrackerUI.Types
import TrackerUI.Commands.Utils

fetchPoints :: MonadIO m => Int -> T.TrackerT m (V.Vector (T.Sensors T.Sample))
fetchPoints n = do
    queue <- T.getSensorQueue
    let go v | V.length v >= n  = return $ V.take n v
             | otherwise        = do v' <- liftIO $ atomically $ readTChan queue
                                     go (v V.++ v')
    go V.empty

exciteCmds :: [Command]
exciteCmds =
    [ command ["excite", "start"]
      "Start excitation" "" $ \_->
        use excitation >>= liftTrackerE . T.configureExcitation . fmap maybeExciteChannel
    , command ["excite", "stop"]
      "Stop excitation" "" $ \_->do
        liftTrackerE $ T.configureExcitation (pure Nothing)
    , command ["excite", "cal"]
      "Run calibration" "" $ \_->do
        samples <- use corrPoints >>= liftTracker . fetchPoints
        let test = fmap (view (T.psd . _x . T.sdDiff)) samples
        decimation <- liftTrackerE $ T.getKnob T.adcDecimation
        decimatedExc <- uses (excitation . _x . excChanExcitation)
                             (T.excitePeriod %~ (/ realToFrac decimation) . realToFrac)
        phaseAmp <- liftTracker $ T.phaseAmp decimatedExc (fmap realToFrac test)
        liftIO $ print phaseAmp
        liftIO $ withFile "r.txt" WriteMode $ \h->
            V.mapM_ (hPutStrLn h . showSensors) samples
    ]

exciteSettings :: [Setting]
exciteSettings =
    concat [ f "x" _x, f "y" _y, f "z" _z ]++
    [ setting "excite.corr-points"
              "number of points to use in correlation"
              stateA corrPoints
    ]
  where f :: String
          -> Lens' (T.Stage ExciteChannel) ExciteChannel
          -> [Setting]
        f n l = [ setting ("excite."++n++".period")
                          "excitation period"
                          stateA
                          (excitation . l . excChanExcitation . T.excitePeriod)
                , setting ("excite."++n++".amp")
                          "excitation amplitude"
                          stateA
                          (excitation . l . excChanExcitation . T.exciteAmp)
                , setting ("excite."++n++".enabled")
                          "excitation enabled"
                          stateA
                          (excitation . l . excChanEnabled)
                ]
