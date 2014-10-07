module TrackerUI.Commands.FineCal
    ( fineCalCmds
    , fineCalSettings
    ) where

import Prelude hiding (concat)
import Data.Maybe
import Data.Foldable as F
import Control.Monad.IO.Class
import qualified Data.Vector as V

import Control.Lens hiding (Setting)

import System.Console.Haskeline

import qualified Tracker as T
import TrackerUI.Types
import TrackerUI.Commands.Utils

fineScanCmd :: Command
fineScanCmd = command ["fine", "scan"] help "" $ \_->do
    points <- use fineScan >>= liftTrackerE . T.fineScan
    lastFineScan ?= points
  where help = "Perform fine calibration scan"

fineCalCmd :: Command
fineCalCmd = command ["fine", "cal"] help "" $ \_->do
    s <- use fineScale
    points <- use lastFineScan >>= tryJust "No fine calibration scan"
    let (psdSetpt, gains) = T.fineCal points
        gains' = over (mapped . mapped . mapped) (realToFrac . (*s)) gains
    liftTrackerE $ do
        T.setKnob T.psdGains gains'
        T.setKnob T.psdSetpoint $ over (mapped . mapped) round psdSetpt
    liftIO $ putStrLn "Feedback gains = "
    liftIO $ putStrLn $ unlines
           $ fmap (F.foldMap (\x->shows x "\t"))
           $ concat $ F.toList $ fmap F.toList gains'
    liftIO $ putStrLn "Feedback setpoint = "
    liftIO $ putStrLn $ concat $ fmap (F.foldMap (\x->shows x "\t")) $ F.toList psdSetpt
  where help = "Perform fine calibration regression"

fineDumpCmd :: Command
fineDumpCmd = command ["fine", "dump"] help "" $ \args->do
    let fname = fromMaybe "fine-cal.txt" $ listToMaybe args
    s <- use lastFineScan >>= tryJust "No fine calibration."
    liftIO $ writeTsv fname $ V.toList s
    liftInputT $ outputStrLn $ "Last fine calibration dumped to "++fname
  where help = "Dump fine calibration points"

fineCalCmds :: [Command]
fineCalCmds =
    [ fineScanCmd
    , fineCalCmd
    , fineDumpCmd
    ]

fineCalSettings :: [Setting]
fineCalSettings = concat
    [ [ pureSetting "fine.points" (Just "number of points in fine calibration")
            readParse show (fineScan . T.fineScanPoints)]
    , r3Setting "fine.range" "size of fine calibration in code points"
            stateA (fineScan . T.fineScanRange . stageV3)
    , [pureSetting "fine.gain-scale" (Just "factor to scale result of regression by to get feedback gains")
            readParse show fineScale]
    ]
