module TrackerUI.Commands.Log (logCmds) where

import Data.Maybe (isJust, fromMaybe)
import Data.Char (ord)
import Data.Int
import Control.Monad (when, forever)
import Control.Monad.IO.Class
import Control.Monad.Error.Class
import System.IO

import Control.Concurrent
import Control.Concurrent.STM

import qualified Data.ByteString.Lazy as BSL
import qualified Data.Vector as V
import qualified Data.Csv as Csv

import Control.Lens hiding (Setting)

import System.Console.Haskeline
import Data.EitherR (fmapLT)
import Control.Error.Util
import qualified Control.Error.Safe as Safe

import qualified Tracker as T
import TrackerUI.Types
import TrackerUI.Commands.Utils

logger :: Handle -> Int -> TChan (V.Vector (T.Sensors Int16)) -> TVar Int -> IO ()
logger h decimation queue countVar = forever $ do
    v <- atomically $ readTChan queue
    let decimated = V.ifilter (\i _->i `mod` decimation == 0) v
    atomically $ modifyTVar countVar (+V.length decimated)
    BSL.hPutStr h $ Csv.encodeWith opts $ V.toList decimated
  where
      opts = Csv.defaultEncodeOptions { Csv.encDelimiter=fromIntegral $ ord '\t' }

killLogger :: Handle -> ThreadId -> TVar Int -> TrackerUI ()
killLogger h thread countVar = do
    liftIO $ killThread thread
    liftIO $ hClose h
    count <- liftIO $ atomically $ readTVar countVar
    liftInputT $ outputStrLn $ "Logged "++show count++" samples"

logStartCmd :: Command
logStartCmd = command ["log","start"] help "FILE [DECIMATION]" $ \args->do
    use stopLogger >>= flip when (throwError "Already logging") . isJust
    fname <- liftEitherT $ Safe.tryAt "Expected filename" args 0
    let dec = fromMaybe 1 $ Safe.atZ args 1 >>= Safe.readZ
    h <- liftEitherT $ fmapLT show $ tryIO $ openFile fname WriteMode
    queue <- liftTracker T.getSensorQueue
    countVar <- liftIO $ newTVarIO 0
    thread <- liftIO $ forkIO $ logger h dec queue countVar
    stopLogger .= Just (killLogger h thread countVar)
    liftInputT $ outputStrLn $ "Logging sensor samples to "++fname
  where help = "Start logging sensor samples to given file"

logStopCmd :: Command
logStopCmd = command ["log","stop"] help "" $ \_->do
    stop <- use stopLogger
    case stop of
        Nothing   -> throwError "Not currently logging"
        Just x    -> x >> stopLogger .= Nothing
  where help = "Stop logging of sensor samples"

logCmds :: [Command]
logCmds = [ logStartCmd, logStopCmd ]
