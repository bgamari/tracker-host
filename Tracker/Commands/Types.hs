{-# LANGUAGE TemplateHaskell #-}

module Tracker.Commands.Types where

import Data.Word
import Control.Lens
import Linear

data CoarseFbChannel
    = CoarseFbChan { _coarseStepHigh  :: V3 Word16
                   , _coarseStepLow   :: V3 Word16
                   , _coarseTolerance :: Word16
                   }
    deriving (Show, Eq, Ord)
    
makeLenses ''CoarseFbChannel
