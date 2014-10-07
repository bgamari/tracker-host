{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TrackerUI.Commands (commands, runCommand) where

import Prelude hiding (concatMap, concat, sequence, mapM_)

import Data.Maybe
import Data.Monoid ((<>))
import Numeric
import Data.Foldable as F
import Data.List (isPrefixOf, stripPrefix, intercalate)
import Control.Applicative
import Control.Monad.State hiding (sequence, forM_, mapM_)
import Control.Monad.Error.Class

import Control.Lens hiding (setting, Setting)
import Linear

import Control.Concurrent.STM

import Control.Error.Util
import Data.EitherR (fmapLT)
import qualified Control.Error.Safe as Safe
import System.Console.Haskeline

import qualified Data.Vector as V

import qualified Tracker as T
import Tracker.Types
import TrackerUI.Types

import TrackerUI.Commands.Utils
import TrackerUI.Commands.PreAmp
import TrackerUI.Commands.Plot
import TrackerUI.Commands.RoughCal
import TrackerUI.Commands.FineCal
import TrackerUI.Commands.Excite
import TrackerUI.Commands.Log

psdChannelNames :: PsdChannels String
psdChannelNames =
    PsdChans $ mkPsd (mkSumDiff "x.sum" "x.diff")
                     (mkSumDiff "y.sum" "y.diff")

tabulatePsdChannels :: ((forall a. Lens' (PsdChannels a) a) -> b)
                    -> PsdChannels b
tabulatePsdChannels f = PsdChans $ mkPsd
    (mkSumDiff (f (_Wrapped' . _x . sdSum)) (f (_Wrapped' . _x . sdDiff)))
    (mkSumDiff (f (_Wrapped' . _y . sdSum)) (f (_Wrapped' . _y . sdDiff)))

exitCmd :: Command
exitCmd = Cmd ["exit"] (Just "Exit the program") "" $ const $ return False

helloCmd :: Command
helloCmd = command ["hello"] help ""
    $ const $ liftInputT $ outputStrLn "hello world!"
  where help = "Print hello world!"

setRawPositionCmd :: Command
setRawPositionCmd = command ["set-pos"] help "(X,Y,Z)" $ \args->do
    pos <- tryHead "expected position" args >>= tryRead "invalid position"
    liftTrackerE $ T.setRawPosition $ pos^.from (stageV3 . v3Tuple)
  where help = "Set raw stage position"

centerCmd :: Command
centerCmd = command ["center"] help "" $ \_->center
  where help = "Set stage at center position"

scanCmd :: Command
scanCmd = command ["scan"] help "[file]" $ \args -> do
    fname <- tryHead "expected file name" args
    rs <- use roughScan
    freq <- use roughScanFreq
    scan <- liftTrackerE $ T.roughScan freq rs
    liftIO $ writeTsv fname $ V.toList scan
    liftInputT $ outputStrLn $ "Scan dumped to "++fname
    center
  where help = "Perform a scan and dump to file (uses rough scan parameters)"

setPsdSetpointCmd :: Command
setPsdSetpointCmd = command ["set-psd-setpoint"] help "" $ \_->do
    liftTrackerE $ do
      s <- lift T.getSensorQueue >>= liftIO . atomically . readTChan
      let sum_ = V.foldl (\acc x->acc ^+^ (x ^. T.psd)) zero s
          n = fromIntegral $ V.length s
      T.setKnob T.psdSetpoint (sum_ & mapped . mapped %~ \x->fromIntegral x `div` n)
  where help = "Set PSD feedback setpoint to current sensor values"

readSensorsCmd :: Command
readSensorsCmd = command ["sensors", "read"] help "" $ \_->do
    let showSensors s = unlines
            [ "Stage = "++F.foldMap (flip showSInt "\t") (s^.T.stage)
            , "PSD   = "++intercalate "\t" [ "x-sum="++showSInt (s^.T.psd._x.sdSum) ""
                                           , "x-diff="++showSInt (s^.T.psd._x.sdDiff) ""
                                           , "y-sum="++showSInt (s^.T.psd._y.sdSum) ""
                                           , "y-diff="++showSInt (s^.T.psd._y.sdDiff) ""
                                           ]
            ]
          where showSInt = showSigned showInt 0
    s <- liftTracker $ T.getSensorQueue >>= liftIO . atomically . readTChan
    liftInputT $ outputStr $ showSensors $ V.head s
  where help = "Read sensors values"

sourceCmd :: Command
sourceCmd = command ["source"] help "FILE" $ \args->do
    fname <- liftEitherT $ Safe.tryAt "Expected filename" args 0
    cmds <- liftEitherT $ fmapLT show $ tryIO $ readFile fname
    mapM_ (runCommand . words) $ lines cmds
  where help = "Execute commands from the given file"

resetCmd :: Command
resetCmd = command ["reset"] help "" $ \_->do
    liftTrackerE T.reset
    -- TODO: Quit or reconnect
  where help = "Perform hardware reset"

eventCountersCmd :: Command
eventCountersCmd = command ["event-counters"] help "" $ \_->do
    liftTrackerE T.getEventCounters >>= liftInputT . outputStrLn . show
  where help = "Show event counters"

helpCmd :: Command
helpCmd = command ["help"] help "[CMD]" $ \args->
    let cmdFilter :: [Command] -> [Command]
        cmdFilter = case args of
                      [] -> id
                      _  -> filter (\c->(c^.cmdName) `isPrefixOf` args)
        cmds = cmdFilter commands
        formatCmd :: Command -> Maybe String
        formatCmd c = case c ^. cmdHelp of
                         Just help -> Just $ take 40 (unwords (c^.cmdName)++" "++c^.cmdArgs++repeat ' ') ++ help
                         Nothing   -> Nothing
    in case cmds of
           []  -> throwError "No matching commands"
           _   -> liftInputT $ outputStr $ unlines $ mapMaybe formatCmd cmds
  where help = "Display help message"

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
    ]

settingCommands :: Setting -> [Command]
settingCommands (Setting {..}) = [getter, setter]
  where get = sAccessors ^. aGet
        put = sAccessors ^. aPut
        getter = Cmd ["get",sName] (("Get "++) <$> sHelp) "" $ \_->
                   get >>= showValue . view sLens >> return True
        setter = Cmd ["set",sName] (("Set "++) <$> sHelp) "VALUE" $ \args->
                   case sParse args of
                     Just value -> do get >>= put . (sLens .~ value)
                                      showValue value
                                      return True
                     Nothing    -> do liftInputT $ outputStrLn
                                           $ "Invalid value: "++unwords args
                                      return True
        showValue value = liftInputT $ outputStrLn $ sName++" = "++sFormat value

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

settings :: [Setting]
settings = concat
    [ roughCalSettings, fineCalSettings, stageSettings, exciteSettings
    , psdSettings, searchSettings, coarseFbSettings
    , [setting "decimation"
               "decimation factor of samples"
               (knobA T.adcDecimation) id]
    , [setting "preamp.optimize.maxVar"
               "Maximum variance allowed in PSD signal"
               stateA preAmpMaxSigma2]
    ]

realDouble :: RealFrac a => Iso' a Double
realDouble = iso realToFrac realToFrac

showCmd :: Command
showCmd = command ["show"] help "PATTERN" $ \args->do
    let matching = filter (\(Setting {..})->isJust sHelp) $
          case listToMaybe args of
            Just pattern -> filter (\(Setting {..})->pattern `isPrefixOf` sName) settings
            Nothing      -> settings
    forM_ matching $ \(Setting {..})->do
        value <- sAccessors^.aGet
        liftInputT $ outputStrLn $ sName++" = "++views sLens sFormat value
  where help = "Show values of settings matching pattern"

commands :: [Command]
commands = [ helloCmd
           , centerCmd
           , setRawPositionCmd
           , scanCmd
           ]
           ++ logCmds ++ roughCalCmds ++ fineCalCmds ++
           [ setPsdSetpointCmd
           , readSensorsCmd
           , sourceCmd
           , resetCmd
           , eventCountersCmd
           , exitCmd
           , helpCmd
           , command ["adc", "start"] "Start ADC triggering" "" $ const
             $ liftTrackerE $ T.setKnob T.adcTriggerMode T.TriggerAuto
           , showCmd
           ] ++ concatMap settingCommands settings
             ++ preAmpCmds
             ++ plotCommands
             ++ exciteCmds
             ++ feedbackCmds

runCommand :: [String] -> TrackerUI Bool
runCommand [] = return True
runCommand input = do
    let cmds = filter (\c->(c^.cmdName) `isPrefixOf` input) commands
    case cmds of
      cmd:[]  -> do let Just rest = stripPrefix (cmd^.cmdName) input
                    catchError (cmd^.cmdAction $ rest) $ \e->do
                        liftInputT $ outputStrLn $ "error: "++e
                        return True
      []      -> do liftInputT $ outputStrLn $ "Unknown command: "++unwords input
                    return True
      _:_     -> do let matches = intercalate ", " $ map (\c->unwords $ c^.cmdName) cmds
                    liftInputT $ outputStrLn $ "Ambiguous command: matches "++matches
                    return True
