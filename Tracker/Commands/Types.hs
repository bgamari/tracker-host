{-# LANGUAGE TemplateHaskell #-}

module Tracker.Commands.Types where

import Data.Word
import Data.Int
import Control.Lens
import Linear

data CoarseFbChannel
    = CoarseFbChan { _coarseStepHigh  :: V3 Int16
                   , _coarseStepLow   :: V3 Int16
                   , _coarseTolerance :: Word16
                   }
    deriving (Show, Eq, Ord)
    
makeLenses ''CoarseFbChannel
