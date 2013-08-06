{-# LANGUAGE OverloadedStrings, PatternGuards, GeneralizedNewtypeDeriving, TemplateHaskell #-}

import Data.Maybe
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.State
import Control.Monad.IO.Class
import Control.Applicative
import Data.Binary.Get
import Data.Int       
       
import qualified Data.Vector as V
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
       
import qualified Tracker as T
import Tracker (TrackerT, Stage(..), Psd(..), Sensors, Sample)
import Linear
import System.Console.Haskeline
import Control.Lens
 
data TrackerState
    = TrackerState { _lastRoughCal :: Maybe (V.Vector (Sensors Sample))
                   }
makeLenses ''TrackerState

newtype TrackerUI a = TUI (StateT TrackerState (InputT (TrackerT IO)) a)
                    deriving ( Functor, Applicative, Monad, MonadIO
                             , MonadState TrackerState )
        
liftInputT :: InputT (TrackerT IO) a -> TrackerUI a
liftInputT = TUI . lift

liftTracker :: TrackerT IO a -> TrackerUI a
liftTracker = TUI . lift . lift

runTrackerUI :: TrackerUI a -> IO (Either String a)
runTrackerUI (TUI a) = T.withTracker $ runInputT defaultSettings $ evalStateT a state0
  where state0 = TrackerState { _lastRoughCal = Nothing
                              }

roughScan :: T.RasterScan
roughScan =
    maybe (error "Invalid scan") id
    $ T.scanAround (pure 0x7fff) (pure 0x1000) (V3 20 20 2)

unitStageGains :: Stage (Stage Int32)
unitStageGains = Stage (Stage 1 0 0) (Stage 0 1 0) (Stage 0 0 1)

main :: IO ()    
main = either error (const $ return ()) =<< go
  where go = runTrackerUI $ do
          liftTracker $ do T.echo "Hello World!" >>= liftIO . print
                           T.setStageGains unitStageGains
                           T.setFeedbackFreq 1000
                           T.setAdcFreq 5000
          while $ prompt
    
while :: Monad m => m Bool -> m ()
while m = m >>= \a->when a (while m)

prompt :: TrackerUI Bool
prompt = do
    line <- liftInputT $ getInputLine "> "
    case maybe ["exit"] words line of
      "exit":_                                 -> return False
      "quit":_                                 -> return False
      cmd:rest | Just action <- lookupCmd cmd  -> action rest >> return True
               | otherwise                     -> do
                   liftInputT $ outputStrLn $ "Unknown command: "++cmd
                   return True
      _         -> return True
  where lookupCmd :: String -> Maybe ([String] -> TrackerUI ())
        lookupCmd cmd = lookup cmd $ map (\(c,_,a)->(c,a)) commands

type Command = (String, String, [String] -> TrackerUI ())

command :: String -> String -> ([String] -> TrackerUI ()) -> Command
command cmd help action = (cmd, help, action)

helloCmd :: Command
helloCmd = command "hello" help $ const $ liftInputT $ outputStrLn "hello world!"
  where help = "Print hello world!"
    
roughCalCmd :: Command
roughCalCmd = command "rough-cal" help $ \args->do
    scan <- liftTracker $ T.roughScan 1000 roughScan
    lastRoughCal .= Just scan
  where help = "Perform rough calibration"
    
dumpRoughCmd :: Command
dumpRoughCmd = command "dump-rough" help $ \args->do
    let fname = fromMaybe "rough-cal.txt" $ listToMaybe args
    scan <- use lastRoughCal
    case scan of
        Nothing -> liftInputT $ outputStrLn "No rough calibration done."
        Just s  -> liftIO $ writeFile fname
                          $ unlines $ map (show . T.stage) $ V.toList s
  where help = "Dump last rough calibration"

commands :: [Command]
commands = [ helloCmd
           , roughCalCmd
           , dumpRoughCmd
           ]
