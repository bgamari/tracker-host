{-# LANGUAGE RankNTypes #-}

module TrackerUI.Commands.PreAmp (preAmpCmds) where

import Prelude hiding (sequence)
import Data.Traversable

import Control.Monad (void)
import Control.Monad.Trans.Maybe
import Control.Error (noteT)

import Linear
import Control.Lens

import PreAmp
import PreAmp.Optimize as PreAmp

import Tracker.Types
import TrackerUI.Types
import TrackerUI.Commands.Utils

openPreAmp :: Command
openPreAmp = command ["preamp", "open"] help "DEVICE" $ \args->do
    device <- tryHead "expected device" args
    pa <- liftEitherT $ PreAmp.open device
    preAmp .= Just pa
  where help = "Open pre-amplifier device"

optimizePreAmp :: Command
optimizePreAmp = command ["preamp", "optimize"] help "" $ \_->do
    pa <- tryJust "No pre-amplifier open" =<< use preAmp
    maxVar <- use preAmpMaxSigma2
    let channel :: (forall a. Lens' (PsdChannels a) a)
                -> TrackerUI (GainOffset CodePoint)
        channel l = do
            liftIO $ putStr $ "Optimizing "++views l show names++": "
            res <- liftTrackerE $ noteT "Failed to optimize"
                                $ MaybeT $ optimize pa l maxVar
            liftIO $ print res
            return res

        names :: PsdChannels String
        names = PsdChans $ mkPsd (mkSumDiff "sumX" "diffX")
                                 (mkSumDiff "sumY" "diffY")

        actions :: PsdChannels (TrackerUI (GainOffset CodePoint))
        actions =
          PsdChans
          $ Psd $ V2 (mkSumDiff (channel $ _Wrapping' PsdChans . _x . sdSum)
                                (channel $ _Wrapping' PsdChans . _x . sdDiff))
                     (mkSumDiff (channel $ _Wrapping' PsdChans . _y . sdSum)
                                (channel $ _Wrapping' PsdChans . _y . sdDiff))
    values <- sequence actions
    preAmpValues .= values
  where help = "Automatically optimize pre amplifier gains and offsets"

resetPreAmp :: Command
resetPreAmp = command ["preamp", "reset"] help "" $ \_->do
    pa <- tryJust "No pre-amplifier open" =<< use preAmp
    let reset ch = PreAmp.setOffset pa ch 0 >> PreAmp.setGain pa ch 0
    void $ liftEitherT $ traverse reset PreAmp.channels
  where help = "Reset pre-amplifier gains and offsets to zero"

preAmpCmds :: [Command]
preAmpCmds = concat [ cmd (_Wrapping' PsdChans . _x . sdSum) "xsum"
                    , cmd (_Wrapping' PsdChans . _x . sdDiff) "xdiff"
                    , cmd (_Wrapping' PsdChans . _y . sdSum) "ysum"
                    , cmd (_Wrapping' PsdChans . _y . sdDiff) "ydiff"
                    ]
             ++ [ openPreAmp, optimizePreAmp, resetPreAmp ]
  where cmd :: (forall a. Lens' (PsdChannels a) a) -> String -> [Command]
        cmd proj name =
            [ Cmd ["set", "amp."++name++".gain"]
                  (Just "Set pre-amplifier gain") "[GAIN]" $ \args -> do
                pa <- use preAmp >>= tryJust "No pre-amplifier open"
                gain <- tryHead "expected gain" args >>= tryRead "invalid gain"
                liftEitherT $ PreAmp.setGain pa ch $ fromIntegral gain
                preAmpValues . proj . PreAmp.gain .= fromIntegral gain
                return True
            , Cmd ["get", "amp."++name++".gain"]
                  (Just "Get pre-amplifier gain") "" $ \_ -> do
                void $ uses (preAmpValues . proj . gain) print
                return True
            , Cmd ["set", "amp."++name++".offset"]
                  (Just "Set pre-amplifier offset") "[OFFSET]" $ \args -> do
                pa <- use preAmp >>= tryJust "No pre-amplifier open"
                offset <- tryHead "expected offset" args >>= tryRead "invalid offset"
                liftEitherT $ PreAmp.setOffset pa ch $ fromIntegral (offset :: Integer)
                preAmpValues . proj . PreAmp.offset .= fromIntegral (offset :: Integer)
                return True
            , Cmd ["get", "amp."++name++".offset"]
                  (Just "Get pre-amplifier offset") "" $ \_ -> do
                void $ uses (preAmpValues . proj . PreAmp.offset) print
                return True
            ]
          where ch = PreAmp.channels ^. proj
