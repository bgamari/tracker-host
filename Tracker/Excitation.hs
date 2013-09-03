module Tracker.Excitation ( Excitation(..)
                          , configureExcitations
                          , defaultExcitations
                          ) where

import Tracker.Types
import Tracker.LowLevel
import Tracker.Commands
import qualified Data.Vector as V
import Data.Int
import Data.List (maximumBy)
import Data.Ord (comparing)
import Control.Applicative
import Data.Traversable as T
import Control.Monad (void)
import Linear

-- | Excitation amplitude
type Amplitude = Double

-- | Phase in number of samples
type Phase = Int

data Excitation = Excitation { excitePeriod :: Int
                             , exciteAmp    :: Amplitude
                             }
                deriving (Show, Eq, Read)

excitation :: Excitation -> V.Vector Int16
excitation (Excitation period amp) = V.generate period f
  where f i = round $ amp * sin (2*pi*realToFrac i/realToFrac period)

configureExcitations :: (Functor m, MonadIO m)
                     => Stage Excitation -> TrackerT m ()
configureExcitations exc =
    void $ T.sequence $ setExcitation <$> stageAxes <*> fmap excitation exc

defaultExcitations :: Stage Excitation
defaultExcitations = mkStage
    (Excitation 239 100)
    (Excitation 397 100)
    (Excitation 163 100)

correlate :: Num a => V.Vector a -> V.Vector a -> Phase -> a
correlate a b lag = (V.drop lag a V.++ a) `dot` b

mean :: Fractional a => V.Vector a -> a
mean v = V.sum v / realToFrac (V.length v)

phaseAmp :: V.Vector Double -> V.Vector Double -> (Phase, Amplitude)
phaseAmp exc samples = 
    let samples' = fmap (\x->x - mean samples) samples
        sampleLen = (V.length samples `div` V.length exc) * V.length exc
        (corrNorm, phase) = maximumBy (comparing fst)
                            [ (correlate (V.take sampleLen samples') exc i, i)
                            | i <- [0..V.length exc `div` 2]
                            ]
        amp = corrNorm / norm exc
    in (phase, amp)
