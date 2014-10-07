module TrackerUI.Commands.Plot (plotCommands) where

import Control.Applicative
import Control.Monad.Error.Class

import Control.Lens

import Tracker.Types
import TrackerUI.Types
import TrackerUI.Commands.Utils
import TrackerUI.Plot

startPlotCmd :: Command
startPlotCmd = command ["plot", "start"] help "" $ \_->do
    plot <- use trackerPlot
    case plot of
      Nothing -> do
        plot' <- startPlot
        trackerPlot .= Just plot'
      Just _  -> do
        throwError $ "Plot already running"
  where help = "Start plot view"

setPlotNPointsCmd :: Command
setPlotNPointsCmd = command ["set", "plot.npoints"] help "" $ \args->do
    plot <- use trackerPlot >>= tryJust "No plot"
    tryHead "Expected number of points" args >>= tryRead "Invalid number of points" >>= liftIO . setNPoints plot
  where help = "Set number of points in plot"

setYSizeCmd :: Command
setYSizeCmd = command ["set", "plot.ysize"] help "" $ \args->do
    plot <- use trackerPlot >>= tryJust "No plot"
    size <- case args of
              []  -> return Nothing
              x:_ -> Just <$> tryRead "Invalid size" x
    liftIO $ setYSize plot size
  where help = "Set Y extent"

plotCommands :: [Command]
plotCommands = [ startPlotCmd
               , setPlotNPointsCmd
               , setYSizeCmd
               ]
