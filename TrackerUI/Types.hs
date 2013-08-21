{-# LANGUAGE GeneralizedNewtypeDeriving, TemplateHaskell #-}

module TrackerUI.Types where

import Data.List (isPrefixOf, stripPrefix)
import Data.Maybe (mapMaybe)
import Data.Word
import Control.Monad.State
import Control.Monad.Trans.Either
import Control.Applicative

import qualified Data.Vector as V
import System.Console.Haskeline
import Control.Lens
import Linear

import qualified Tracker as T
import PreAmp
import Tracker ( TrackerT, Stage(..), Psd(..), Sensors, Sample
               , RasterScan(..), FineScan(..))

data TrackerState
    = TrackerState { _lastRoughCal   :: Maybe (V.Vector (Sensors Sample))
                   , _roughScanFreq  :: Word32
                   , _roughScan      :: RasterScan Stage Word16
                   , _fineScan       :: FineScan
                   , _feedbackGains  :: Psd (Stage Double)
                   , _preAmp         :: Maybe PreAmp
                   }
makeLenses ''TrackerState
           
defaultTrackerState :: TrackerState           
defaultTrackerState =
    TrackerState { _lastRoughCal  = Nothing
                 , _roughScanFreq = 1000
                 , _roughScan     = RasterScan { _scanCenter = pure 0x7fff
                                               , _scanSize   = pure 0x1000
                                               , _scanPoints = Stage $ V3 20 20 2
                                               }
                 , _fineScan      = FineScan { _fineScanRange  = pure 0x500
                                             , _fineScanCenter = pure 0x7fff
                                             , _fineScanPoints = 500
                                             , _fineScanFreq   = 1000
                                             }
                 , _feedbackGains = pure $ pure 0
                 , _preAmp        = Nothing
                 }

newtype TrackerUI a = TUI (StateT TrackerState (InputT (TrackerT IO)) a)
                    deriving ( Functor, Applicative, Monad, MonadIO
                             , MonadState TrackerState )

liftInputT :: InputT (TrackerT IO) a -> TrackerUI a
liftInputT = TUI . lift

liftTracker :: TrackerT IO a -> TrackerUI a
liftTracker = TUI . lift . lift

data Command = Cmd { _cmdName   :: [String]
                   , _cmdHelp   :: Maybe String
                   , _cmdArgs   :: String
                   , _cmdAction :: [String] -> EitherT String TrackerUI Bool
                   }
makeLenses ''Command

completeCommand :: MonadIO m => [Command] -> CompletionFunc m
completeCommand commands (left, right) = do
    let tokens = words (reverse left)++if ' ' == head left then [""] else []
    return (left, completions [(c^.cmdName, c) | c <- commands] tokens)
  where completions :: [([String], Command)] -> [String] -> [Completion]
        completions cmds [] = [ Completion (c ^. _1 . _head) (c ^. _1 . _head) True | c <- cmds ]
        completions cmds [token] =
            let matching = mapMaybe (\c->case stripPrefix token (c ^. _1 . _head) of
                                           Just x   -> Just $ c & _1 .~ (x, c ^. _1 . _head)
                                           Nothing  -> Nothing
                                    )
                         $ filter (\c->not $ c ^. _1 . to null)
                         $ cmds
            in [ Completion (c ^. _1 . _1) (c ^. _1 . _2) True | c <- matching ]
        completions cmds (token:tokens) =
            let matching = filter (\c->token == (c ^. _1 . _head))
                         $ filter (\c->not $ c ^. _1 . to null)
                         $ cmds
            in completions (matching & mapped . _1 %~ tail) tokens
    
runTrackerUI :: [Command] -> TrackerUI a -> IO (Either String a)
runTrackerUI commands (TUI a) =
    T.withTracker $ runInputT settings $ evalStateT a defaultTrackerState
  where settings = Settings { complete    = completeCommand commands
                            , historyFile = Just "~/.tracker.history"
                            , autoAddHistory = True
                            }

