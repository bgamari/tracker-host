{-# LANGUAGE GeneralizedNewtypeDeriving, TemplateHaskell, RankNTypes, ExistentialQuantification #-}

module TrackerUI.Types where

import Data.Function (on)
import Data.List (isPrefixOf, stripPrefix, sortBy, nubBy)
import Data.Maybe (mapMaybe)
import Data.Word
import Control.Monad.Error.Class
import Control.Monad.State
import Control.Monad.Trans.Either
import Control.Applicative
import Control.Concurrent (ThreadId)

import qualified Data.Vector as V
import System.Console.Haskeline
import Control.Lens hiding (Setting)
import Linear

import qualified Tracker as T
import TrackerUI.Plot.Types
import PreAmp
import Tracker ( TrackerT, Stage(..), Psd(..), Sensors, Sample
               , RasterScan(..), FineScan(..))

data ExciteChannel = ExcChan { _excChanEnabled :: Bool
                             , _excChanExcitation :: T.Excitation
                             }
                   deriving (Show, Read)
makeLenses ''ExciteChannel     

maybeExciteChannel :: ExciteChannel -> Maybe T.Excitation
maybeExciteChannel (ExcChan True exc) = Just exc
maybeExciteChannel _                  = Nothing

newtype TrackerUI a = TUI (EitherT String (StateT TrackerState (InputT (TrackerT IO))) a)
                    deriving ( Functor, Applicative, Monad, MonadIO
                             , MonadState TrackerState, MonadError String
                             )

liftEitherT :: EitherT String (StateT TrackerState (InputT (TrackerT IO))) a -> TrackerUI a
liftEitherT = TUI            

liftInputT :: InputT (TrackerT IO) a -> TrackerUI a
liftInputT = TUI . lift . lift

liftTracker :: TrackerT IO a -> TrackerUI a
liftTracker = TUI . lift . lift . lift

liftTrackerE :: EitherT String (TrackerT IO) a -> TrackerUI a
liftTrackerE m = liftTracker (runEitherT m) >>= liftEitherT . either left right

data TrackerState
    = TrackerState { _lastRoughCal   :: Maybe (V.Vector (Sensors Sample))
                   , _roughScanFreq  :: Word32
                   , _roughScan      :: RasterScan Stage Word16
                   , _fineScan       :: FineScan
                   , _feedbackGains  :: Psd (Stage Double)
                   , _preAmp         :: Maybe PreAmp
                   , _stopLogger     :: Maybe (TrackerUI ())
                   , _trackerPlot    :: Maybe TrackerPlot
                   , _corrPoints     :: Int
                   , _excitation     :: Stage ExciteChannel
                   }
makeLenses ''TrackerState
           
defaultTrackerState :: TrackerState           
defaultTrackerState =
    TrackerState { _lastRoughCal  = Nothing
                 , _roughScanFreq = 1000
                 , _roughScan     = RasterScan { _scanCenter = pure 0x7fff
                                               , _scanSize   = pure 4000
                                               , _scanPoints = Stage $ V3 40 40 1
                                               }
                 , _fineScan      = FineScan { _fineScanRange  = pure 0x500
                                             , _fineScanCenter = pure 0x7fff
                                             , _fineScanPoints = 500
                                             , _fineScanFreq   = 2000
                                             }
                 , _feedbackGains = pure $ pure 0
                 , _preAmp        = Nothing
                 , _stopLogger    = Nothing
                 , _trackerPlot   = Nothing
                 , _corrPoints    = 4000
                 , _excitation    = fmap (ExcChan False) T.defaultExcitation
                 }

data Accessors m a = Accessors { _aGet :: m a
                               , _aPut :: a -> m ()
                               }
makeLenses ''Accessors

stateA :: Accessors TrackerUI TrackerState
stateA = Accessors get put

knobA :: T.Knob a -> Accessors TrackerUI a
knobA knob = Accessors (liftTrackerE $ T.getKnob knob)
                       (liftTrackerE . T.setKnob knob)

data Setting = forall a s. Setting { sName      :: String
                                   , sHelp      :: Maybe String
                                   , sParse     :: [String] -> Maybe a
                                   , sFormat    :: a -> String
                                   , sAccessors :: Accessors TrackerUI s
                                   , sLens      :: Lens' s a
                                   }
     
pureSetting :: String -> Maybe String -> ([String] -> Maybe a) -> (a -> String) -> Lens' TrackerState a -> Setting
pureSetting name help parse format l =
    Setting name help parse format stateA l

data Command = Cmd { _cmdName   :: [String]
                   , _cmdHelp   :: Maybe String
                   , _cmdArgs   :: String
                   , _cmdAction :: [String] -> TrackerUI Bool
                   }
makeLenses ''Command

sortNubOn :: (Eq b, Ord b) => (a -> b) -> [a] -> [a]
sortNubOn f = nubBy ((==) `on` f) . sortBy (compare `on` f)          

completeCommand :: MonadIO m => [Command] -> CompletionFunc m
completeCommand commands (left, right) = do
    let tokens = words (reverse left)++if ' ' == head left then [""] else []
    return ( left
           , sortNubOn replacement $ completions [(c^.cmdName, c) | c <- commands] tokens)
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
    join <$> ( T.withTracker
             $ runInputT settings
             $ flip evalStateT defaultTrackerState
             $ runEitherT a)
  where settings = Settings { complete    = completeCommand commands
                            , historyFile = Just "~/.tracker.history"
                            , autoAddHistory = True
                            }

