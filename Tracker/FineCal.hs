{-# LANGUAGE TemplateHaskell #-}                

module Tracker.FineCal (fineCal) where

import Data.Traversable as T
import Control.Applicative
import Control.Lens
import Control.Monad
import Data.Word
import Data.Int
import qualified Data.Vector as V
import Linear
import System.Random.MWC

import Tracker.Types
import Tracker.Monad
import Tracker.PathAcquire

data FineCal = FineCal { _fineScanRange   :: Stage Word16
                       , _fineScanCenter  :: Stage Word16
                       , _fineScanPoints  :: Int
                       , _fineScanFreq    :: Word32
                       }
             deriving (Show)
makeLenses ''FineCal

fineCal :: (Applicative m, MonadIO m)
        => FineCal -> TrackerT m (Psd (Stage Double))
fineCal fineCal = do
    points <- fineScan fineCal
    return undefined
   
fineScan :: (Applicative m, MonadIO m)
         => FineCal -> TrackerT m (V.Vector (Sensors Int16))
fineScan fineCal = do
    path <- liftIO $ withSystemRandom $ asGenIO $ \mwc->do
        let point = T.sequence $ pure (uniform mwc)
        replicateM (fineCal ^. fineScanPoints) (Stage <$> point)
    pathAcquire (fineCal^.fineScanFreq) path
