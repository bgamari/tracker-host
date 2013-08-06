module Tracker.LowLevel
    ( Tracker
    , open
    , close
    , writeCommand
    , readReply
    , parseReply
    , readAck
    , readData
    ) where

import System.USB
import Control.Applicative
import Data.Word
import Data.Binary.Put
import Data.Binary.Get
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Vector as V

import Numeric
import Data.Foldable (toList)

data Tracker = Tracker DeviceHandle

trackerVendor = 0x1d50 :: VendorId
trackerProduct = 0x7777 :: ProductId

findDevice :: Ctx -> VendorId -> ProductId -> IO (V.Vector Device)
findDevice ctx vid pid = do
    V.filterM go =<< getDevices ctx
  where go :: Device -> IO Bool
        go dev = do desc <- getDeviceDesc dev
                    return $ deviceVendorId desc == vid && deviceProductId desc == pid

-- | Open the tracker device
open :: IO (Maybe Tracker)
open = do
    ctx <- newCtx
    devices <- findDevice ctx trackerVendor trackerProduct
    case toList devices of
      []     -> return Nothing
      dev:_  -> Just <$> open' (V.head devices)
    
open' :: Device -> IO Tracker
open' device = do
    h <- openDevice device
    setConfig h (Just 1)
    claimInterface h 0
    return $ Tracker h

-- | Close the tracker device
close :: Tracker -> IO ()
close (Tracker h) = closeDevice h

cmdInEndpt = EndpointAddress 0x1 In
cmdOutEndpt = EndpointAddress 0x2 Out
dataInEndpt = EndpointAddress 0x3 In
cmdTimeout = 100
dataTimeout = 100

showByteString :: BS.ByteString -> String
showByteString = concatMap (flip showHex " " . fromIntegral) . BS.unpack

writeCommand :: Tracker -> Word8 -> Put -> IO ()
writeCommand (Tracker h) cmd payload = do
    let frame = BSL.toStrict $ runPut $ putWord8 cmd >> payload
    putStrLn $ "> "++showByteString (BS.take 4 frame)
    (size, status) <- writeBulk h cmdOutEndpt frame cmdTimeout
    case status of
        TimedOut  -> error "Command write timed out"
        Completed -> return ()

readReply :: Tracker -> IO (Maybe ByteString)
readReply (Tracker h) = do
    (d, status) <- readBulk h cmdInEndpt 512 cmdTimeout
    putStrLn $ "< "++showByteString (BS.take 4 d)
    let cmd = BS.head d
        statusCode = BS.head $ BS.drop 1 d
    case status of
        TimedOut  -> error "Reply read timed out"
        _ | BS.length d < 2    -> error "Too short reply"
          | statusCode == 0x06 -> return $ Just $ BS.drop 2 d
          | otherwise          -> return Nothing
                       
readAck :: Tracker -> String -> IO ()
readAck tracker when = do
    a <- readReply tracker
    case a of
        Just _  -> return ()
        Nothing -> error $ "Ack expected: "++when

parseReply :: Tracker -> Get a -> IO (Maybe a)
parseReply tracker parser = do
     reply <- readReply tracker
     return $ case reply of
         Nothing    -> Nothing
         Just reply -> either (const Nothing) (\(_,_,a)->Just a)
                     $ runGetOrFail parser $ BSL.fromStrict reply

readData :: Tracker -> IO (Maybe ByteString)
readData (Tracker h) = do
    (d, status) <- readBulk h dataInEndpt 512 dataTimeout
    case status of
        TimedOut  -> return Nothing
        Completed -> return $ Just d
