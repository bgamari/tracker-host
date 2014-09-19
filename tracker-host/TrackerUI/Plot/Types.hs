{-# LANGUAGE TemplateHaskell #-}
                
module TrackerUI.Plot.Types where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Lens
import Data.Int

data PlotConfig = PlotConfig { _pcYSize       :: Maybe Int16
                             , _pcNPoints     :: Int
                             , _pcDecimation  :: Int
                             }
                deriving (Show)
makeLenses ''PlotConfig

data TrackerPlot = TrackerPlot { _tpWorker   :: ThreadId
                               , _tpConfig   :: TVar PlotConfig
                               }
makeLenses ''TrackerPlot
