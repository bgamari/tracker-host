{-# LANGUAGE GeneralizedNewtypeDeriving, TemplateHaskell #-}

module TrackerUI.Types where

import Data.Word
import Control.Monad.State
import Control.Applicative

import qualified Data.Vector as V
import System.Console.Haskeline
import Control.Lens
import Linear

import qualified Tracker as T
import Tracker (TrackerT, Stage(..), Psd(..), Sensors, Sample, RasterScan(..))

data TrackerState
    = TrackerState { _lastRoughCal :: Maybe (V.Vector (Sensors Sample))
                   , _roughScan    :: T.RasterScan Stage Word16
                   }
makeLenses ''TrackerState
           
defaultTrackerState :: TrackerState           
defaultTrackerState =
    TrackerState { _lastRoughCal = Nothing
                 , _roughScan    = RasterScan { _scanCenter = pure 0x7fff
                                              , _scanSize   = pure 0x1000
                                              , _scanPoints = Stage $ V3 20 20 2
                                              }
                 }

newtype TrackerUI a = TUI (StateT TrackerState (InputT (TrackerT IO)) a)
                    deriving ( Functor, Applicative, Monad, MonadIO
                             , MonadState TrackerState )

liftInputT :: InputT (TrackerT IO) a -> TrackerUI a
liftInputT = TUI . lift

liftTracker :: TrackerT IO a -> TrackerUI a
liftTracker = TUI . lift . lift

runTrackerUI :: TrackerUI a -> IO (Either String a)
runTrackerUI (TUI a) =
    T.withTracker $ runInputT defaultSettings $ evalStateT a defaultTrackerState

data Command = Cmd { _cmdName   :: [String]
                   , _cmdHelp   :: String
                   , _cmdArgs   :: String
                   , _cmdAction :: [String] -> TrackerUI Bool
                   }
makeLenses ''Command
