{-# LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving #-}

module Tracker.LowLevel
    ( TrackerT
    , withTracker
    , SensorQueue
    , getSensorQueue
    , CmdId
    , writeCommand
    , readReply
    , parseReply
    , readAck
    , liftThrough
      -- * Convenient re-exports
    , MonadIO
    ) where

import System.USB
import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad (liftM, forever, when)
import Data.Word
import Data.Int
import Data.Traversable
import Data.List (intercalate)
import Data.Foldable (toList)
import Data.Binary.Put
import Data.Binary.Get
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Vector as V

import Control.Error
import System.Console.Haskeline.MonadException
import Control.Monad.Morph
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Class
import Control.Concurrent.STM
import Control.Concurrent.Async
import Numeric
import Control.Lens
import Linear
import Tracker.Types

type SensorQueue = TChan (V.Vector (Sensors Int16))

type CmdId = Word8

data Env = Env { _device         :: DeviceHandle
               , _sensorQueue    :: SensorQueue
               }
makeLenses ''Env

newtype TrackerT m a = TrackerT {getTrackerT :: ReaderT Env m a}
                     deriving ( Functor, Applicative, Monad , MonadIO, MonadException )

instance MonadTrans TrackerT where
    lift = TrackerT . lift

instance MFunctor TrackerT where
    hoist f (TrackerT m) = TrackerT $ ReaderT $ \r->f $ runReaderT m r

-- | This is required for async which is monomorphic in IO
liftThrough :: MonadIO m => (IO a -> IO b) -> TrackerT IO a -> TrackerT m b
liftThrough f (TrackerT a) = TrackerT $ do
    r <- ask
    liftIO $ f $ runReaderT a r

trackerVendor = 0x1d50 :: VendorId
trackerProduct = 0x7777 :: ProductId

findDevice :: Ctx -> VendorId -> ProductId -> IO (V.Vector Device)
findDevice ctx vid pid = do
    V.filterM go =<< getDevices ctx
  where go :: Device -> IO Bool
        go dev = do desc <- getDeviceDesc dev
                    return $ deviceVendorId desc == vid && deviceProductId desc == pid

withDevice :: Monad m => (DeviceHandle -> m a) -> TrackerT m a
withDevice f = TrackerT (view device) >>= lift . f

withDeviceIO :: MonadIO m => (DeviceHandle -> IO a) -> TrackerT m a
withDeviceIO f = TrackerT (view device) >>= liftIO . f

runTrackerT :: MonadIO m => TrackerT m a -> DeviceHandle -> m a
runTrackerT (TrackerT m) h = do
    queue <- liftIO $ newBroadcastTChanIO
    let env = Env h queue
    thread <- liftIO $ async $ runReaderT (getTrackerT sensorListen) env
    r <- runReaderT m env
    liftIO $ cancel thread
    return r

-- | Open the tracker device
withTracker :: MonadIO m => TrackerT m a -> m (Either String a)
withTracker m = do
    ctx <- liftIO newCtx
    devices <- liftIO $ findDevice ctx trackerVendor trackerProduct
    case toList devices of
      []     -> return $ Left "No device found"
      dev:_  -> Right `liftM` withTracker' dev m

withTracker' :: MonadIO m => Device -> TrackerT m a -> m a
withTracker' device m = do
    h <- liftIO $ openDevice device
    liftIO $ setConfig h (Just 1)
    liftIO $ claimInterface h 0
    a <- runTrackerT m h
    liftIO $ closeDevice h
    return a

getSensorQueue :: MonadIO m => TrackerT m SensorQueue
getSensorQueue = TrackerT (view sensorQueue) >>= liftIO . atomically . dupTChan

getInt16le :: Get Int16
getInt16le = fromIntegral `fmap` getWord16le

parseFrames :: BS.ByteString -> V.Vector (Sensors Sample)
parseFrames a =
    runGet (V.replicateM (BS.length a `div` 16) frame) $ BSL.fromStrict a
  where frame = do stage <- sequenceA $ pure getInt16le :: Get (Stage Sample)
                   xSum  <- getInt16le
                   xDiff <- getInt16le
                   ySum  <- getInt16le
                   yDiff <- getInt16le
                   _     <- getInt16le
                   let sumDiff = Psd $ V2 (mkSumDiff xSum xDiff) (mkSumDiff ySum yDiff)
                   return $ Sensors stage sumDiff

sensorListen :: MonadIO m => TrackerT m ()
sensorListen = forever $ do
    d <- either error id `liftM` runEitherT readData
    queue <- TrackerT $ view sensorQueue
    case d of
        Just d' -> liftIO $ atomically $ writeTChan queue $ parseFrames d'
        Nothing -> return ()

cmdInEndpt = EndpointAddress 0x1 In
cmdOutEndpt = EndpointAddress 0x2 Out
dataInEndpt = EndpointAddress 0x3 In
cmdTimeout = 500   -- milliseconds
dataTimeout = 100  -- milliseconds

showByteString :: BS.ByteString -> String
showByteString = intercalate " " . map (pad 2 . flip showHex "" . fromIntegral) . BS.unpack
  where pad :: Int -> String -> String
        pad n = reverse . take n . (++repeat '0') . reverse

debugOut :: MonadIO m => String -> m ()
--debugOut = liftIO . putStrLn
debugOut _ = return ()

writeCommand :: MonadIO m => CmdId -> Put -> EitherT String (TrackerT m) ()
writeCommand cmd payload = EitherT $ withDeviceIO $ \h->do
    let frame = BSL.toStrict $ runPut $ putWord8 cmd >> payload
    debugOut $ "   > "++showByteString (BS.take 200 frame)
    when (BS.length frame > 512)
        $ error $ "writeCommand: Frame too long: "++show (BS.length frame)
    (size, status) <- liftIO $ writeBulk h cmdOutEndpt frame cmdTimeout
    case status of
        TimedOut  -> return $ Left "Command write timed out"
        Completed -> return $ Right ()

readReply :: MonadIO m => EitherT String (TrackerT m) (Maybe ByteString)
readReply = EitherT $ withDeviceIO $ \h->do
    (d, status) <- readBulk h cmdInEndpt 512 cmdTimeout
    debugOut $ "   < "++showByteString (BS.take 32 d)
    let cmd = BS.head d
        statusCode = BS.head $ BS.drop 1 d
    return $ case status of
        TimedOut               -> Left "Reply read timed out"
        _ | BS.length d < 2    -> Left "Reply too short"
          | statusCode == 0x06 -> Right $ Just $ BS.drop 2 d
          | otherwise          -> Right Nothing

readAck :: MonadIO m => String -> EitherT String (TrackerT m) ()
readAck when = do
    a <- readReply
    case a of
        Just _  -> return ()
        Nothing -> left $ when++": ACK expected"

parseReply :: MonadIO m => Get a -> EitherT String (TrackerT m) (Maybe a)
parseReply parser = do
     reply <- readReply
     return $ case reply of
         Nothing    -> Nothing
         Just reply -> either (const Nothing) (\(_,_,a)->Just a)
                     $ runGetOrFail parser $ BSL.fromStrict reply

readData :: MonadIO m => EitherT String (TrackerT m) (Maybe ByteString)
readData = EitherT $ withDeviceIO $ \h->do
    (d, status) <- readBulk h dataInEndpt 512 dataTimeout
    debugOut $ "   % "++showByteString (BS.take 32 d)
    return $ Right $ case status of
        TimedOut  -> Nothing
        Completed -> Just d
