{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TrackerUI.Commands (commands, runCommand) where

import Prelude hiding (concatMap, concat, sequence, mapM_)

import Data.Maybe
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
import TrackerUI.Commands.Feedback
import TrackerUI.Commands.Stage
import TrackerUI.Commands.Trap

exitCmd :: Command
exitCmd = Cmd ["exit"] (Just "Exit the program") "" $ const $ return False

helloCmd :: Command
helloCmd = command ["hello"] help ""
    $ const $ liftInputT $ outputStrLn "hello world!"
  where help = "Print hello world!"

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

settings :: [Setting]
settings = concat
    [ roughCalSettings, fineCalSettings, stageSettings, exciteSettings
    , feedbackSettings
    , trapSettings
    , [setting "decimation"
               "decimation factor of samples"
               (knobA T.adcDecimation) id]
    , [setting "preamp.optimize.maxVar"
               "Maximum variance allowed in PSD signal"
               stateA preAmpMaxSigma2]
    ]

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
           , scanCmd
           ]
           ++ stageCmds ++ logCmds ++ roughCalCmds ++ fineCalCmds
           ++ feedbackCmds ++
           [ readSensorsCmd
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
             ++ trapCmds

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
