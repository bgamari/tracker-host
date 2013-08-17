module Tracker.LowLevel
    ( TrackerT
    , withTracker
    , writeCommand
    , readReply
    , parseReply
    , readAck
    , readData
    ) where

import System.USB
import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad (liftM)
import Data.Word
import Data.List (intercalate)
import Data.Binary.Put
import Data.Binary.Get
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Vector as V

import Numeric
import Data.Foldable (toList)

import Tracker.Monad

trackerVendor = 0x1d50 :: VendorId
trackerProduct = 0x7777 :: ProductId

findDevice :: Ctx -> VendorId -> ProductId -> IO (V.Vector Device)
findDevice ctx vid pid = do
    V.filterM go =<< getDevices ctx
  where go :: Device -> IO Bool
        go dev = do desc <- getDeviceDesc dev
                    return $ deviceVendorId desc == vid && deviceProductId desc == pid

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

cmdInEndpt = EndpointAddress 0x1 In
cmdOutEndpt = EndpointAddress 0x2 Out
dataInEndpt = EndpointAddress 0x3 In
cmdTimeout = 100
dataTimeout = 100

showByteString :: BS.ByteString -> String
showByteString = intercalate " " . map (pad 2 . flip showHex "" . fromIntegral) . BS.unpack
  where pad :: Int -> String -> String
        pad n = reverse . take n . (++repeat '0') . reverse

debugOut :: MonadIO m => String -> m ()
--debugOut = liftIO . putStrLn
debugOut _ = return ()

writeCommand :: MonadIO m => Word8 -> Put -> TrackerT m ()
writeCommand cmd payload = withDeviceIO $ \h->do
    let frame = BSL.toStrict $ runPut $ putWord8 cmd >> payload
    debugOut $ "   > "++showByteString (BS.take 200 frame)
    (size, status) <- liftIO $ writeBulk h cmdOutEndpt frame cmdTimeout
    case status of
        TimedOut  -> error "Command write timed out"
        Completed -> return ()

readReply :: MonadIO m => TrackerT m (Maybe ByteString)
readReply = withDeviceIO $ \h->do
    (d, status) <- readBulk h cmdInEndpt 512 cmdTimeout
    debugOut $ "   < "++showByteString (BS.take 32 d)
    let cmd = BS.head d
        statusCode = BS.head $ BS.drop 1 d
    case status of
        TimedOut  -> error "Reply read timed out"
        _ | BS.length d < 2    -> error "Too short reply"
          | statusCode == 0x06 -> return $ Just $ BS.drop 2 d
          | otherwise          -> return Nothing
                       
readAck :: MonadIO m => String -> TrackerT m ()
readAck when = do
    a <- readReply
    case a of
        Just _  -> return ()
        Nothing -> error $ "Ack expected: "++when

parseReply :: MonadIO m => Get a -> TrackerT m (Maybe a)
parseReply parser = do
     reply <- readReply
     return $ case reply of
         Nothing    -> Nothing
         Just reply -> either (const Nothing) (\(_,_,a)->Just a)
                     $ runGetOrFail parser $ BSL.fromStrict reply

readData :: MonadIO m => TrackerT m (Maybe ByteString)
readData = withDeviceIO $ \h->do
    (d, status) <- readBulk h dataInEndpt 512 dataTimeout
    debugOut $ "   % "++showByteString (BS.take 32 d)
    case status of
        TimedOut  -> return Nothing
        Completed -> return $ Just d
