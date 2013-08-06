{-# LANGUAGE OverloadedStrings, PatternGuards, GeneralizedNewtypeDeriving #-}

import Data.Maybe
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import Control.Applicative
import Data.Binary.Get
import Data.Int       
       
import qualified Data.Vector as V
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
       
import qualified Tracker as T
import Tracker (TrackerT, Stage(..), Psd(..))
import Linear
import System.Console.Haskeline
 
newtype TrackerUI a = TUI {runTrackerUI :: InputT (TrackerT IO) a}
                    deriving ( Functor, Applicative, Monad, MonadIO )
        
liftInputT :: InputT (TrackerT IO) a -> TrackerUI a
liftInputT = TUI

liftTracker :: TrackerT IO a -> TrackerUI a
liftTracker = TUI . lift

roughScan :: T.RasterScan
roughScan =
    maybe (error "Invalid scan") id
    $ T.scanAround (pure 0x7fff) (pure 0x1000) (V3 20 20 2)

unitStageGains :: Stage (Stage Int32)
unitStageGains = Stage (Stage 1 0 0) (Stage 0 1 0) (Stage 0 0 1)

main :: IO ()    
main = either error (const $ return ()) =<< go
  where go = T.withTracker $ runInputT defaultSettings $ runTrackerUI $ do
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
      "exit":_                                      -> return False
      "quit":_                                      -> return False
      cmd:rest | Just action <- lookup cmd commands -> action rest >> return True
               | otherwise                          -> do
                   liftInputT $ outputStrLn $ "Unknown command: "++cmd
                   return True
      _         -> return True

type Handler = [String] -> TrackerUI ()

roughCal :: Handler
roughCal args = do
    scan <- liftTracker $ T.roughScan 1000 roughScan
    V.forM_ scan (liftIO . putStrLn . show . fst)

commands :: [(String, Handler)]
commands = [ ("hello",         const $ liftInputT $ outputStrLn "hello")
           , ("rough-cal",     roughCal)
           ]
