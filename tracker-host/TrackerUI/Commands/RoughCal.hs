module TrackerUI.Commands.RoughCal (roughCalSettings, roughCalCmds) where

import Data.Maybe

import Control.Lens hiding (setting, Setting)
import Linear

import qualified Data.Vector as V

import System.Console.Haskeline

import qualified Tracker as T
import Tracker.RoughCal.Model as Model
import Tracker.Types
import TrackerUI.Types
import TrackerUI.Commands.Utils

roughScanCmd :: Command
roughScanCmd = command ["rough", "scan"] help "" $ \_->do
    rs <- uses roughScan $ (T.scanSize . _z .~ 0)
                         . (T.scanPoints . _z .~ 1)
    freq <- use roughScanFreq
    scan <- liftTrackerE $ T.roughScan freq $ T.rasterScanToPath rs
    lastRoughScan .= Just scan
    center
  where help = "Perform rough scan"

roughCenterCmd :: Command
roughCenterCmd = command ["rough", "center"] help "" $ \_->do
    scan <- use lastRoughScan >>= tryJust "No last rough calibration"
    let c = T.roughCenter scan
    liftInputT $ outputStrLn $ show c
    roughScan . T.scanCenter .= fmap round c
    fineScan . T.fineScanCenter .= fmap round c
    centerPos .= fmap round c
    center
  where help = "Find center of particle from XY rough scan"

roughZScanCmd :: Command
roughZScanCmd = command ["rough", "zscan"] help "" $ \_->do
    rs <- uses roughScan $ (T.scanSize . _y .~ 0)
                         . (T.scanPoints . _y .~ 1)
    freq <- use roughScanFreq
    zScan <- liftTrackerE $ T.roughScan freq $ T.rasterScanToPath rs
    lastRoughZScan .= Just zScan
    center
  where help = "Perform rough Z scan"

roughFitCmd :: Command
roughFitCmd = command ["rough", "fit"] help "" $ \_->do
    scan <- use lastRoughScan >>= tryJust "No rough calibration"
    let samples = V.map (\s->(s^.stage._xy, s^.psd._x.sdDiff))
                  $ V.map (fmap realToFrac) scan
        m0 = Model.initialModel samples :: Model.Model V2 Double
        m = head $ drop 10 $ Model.fit samples m0
        --center = Model.modelCenter m
        --gains = Model.modelToGains center m
    liftIO $ print m0
    liftIO $ print m
    --liftIO $ print gains
    --when ("gains" `elem` args)
    --  $ liftTrackerE $ T.setKnob T.psdGains gains
  where help = "Perform fit on rough calibration"

dumpRoughCmd :: Command
dumpRoughCmd = command ["rough", "dump"] help "[FILENAME]" $ \args->do
    let fname = fromMaybe "rough-cal.txt" $ listToMaybe args
    s <- use lastRoughScan >>= tryJust "No rough calibration."
    liftIO $ writeTsv fname $ V.toList s
    liftInputT $ outputStrLn $ "Last rough calibration dumped to "++fname
  where help = "Dump last rough calibration"

dumpZRoughCmd :: Command
dumpZRoughCmd = command ["rough", "zdump"] help "[FILENAME]" $ \args->do
    let fname = fromMaybe "rough-zcal.txt" $ listToMaybe args
    s <- use lastRoughZScan >>= tryJust "No rough Z calibration."
    liftIO $ writeTsv fname $ V.toList s
    liftInputT $ outputStrLn $ "Last rough calibration dumped to "++fname
  where help = "Dump last rough Z calibration"


roughCalSettings :: [Setting]
roughCalSettings = concat
    [ r3Setting "rough.size" "rough calibration field size in code-points"
            stateA (roughScan . T.scanSize . stageV3)
    , r3Setting "rough.center" "rough calibration field center in code-points"
            stateA (roughScan . T.scanCenter . stageV3)
    , r3Setting "rough.points" "number of points in rough calibration scan"
            stateA (roughScan . T.scanPoints . stageV3)
    , [pureSetting "rough.freq" (Just "update frequency of rough calibration scan")
            readParse show roughScanFreq]
    ]

roughCalCmds :: [Command]
roughCalCmds =
    [ roughScanCmd
    , roughCenterCmd
    , roughZScanCmd
    , roughFitCmd
    , dumpRoughCmd
    , dumpZRoughCmd
    ]
