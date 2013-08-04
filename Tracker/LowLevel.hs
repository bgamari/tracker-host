module Tracker.LowLevel
    ( Tracker
    , open
    , close
    , writeCommand
    , readReply
    , readAck
    ) where

import System.USB
import Control.Applicative
import Data.Word
import Data.Binary.Put
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Vector as V

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
    if V.null devices
      then return Nothing
      else do h <- openDevice $ V.head devices
              setConfig h (Just 1)
              claimInterface h 0
              return $ Just $ Tracker h

-- | Close the tracker device
close :: Tracker -> IO ()
close (Tracker h) = closeDevice h

cmdInEndpt = EndpointAddress 0x1 In
cmdOutEndpt = EndpointAddress 0x2 Out
dataInEndpt = EndpointAddress 0x3 In
cmdTimeout = 100

writeCommand :: Tracker -> Word8 -> Put -> IO ()
writeCommand (Tracker h) cmd payload = do
    let frame = BSL.toStrict $ runPut $ putWord8 cmd >> payload
    (size, status) <- writeBulk h cmdOutEndpt frame cmdTimeout
    case status of
        TimedOut  -> error "Command write timed out"
        Completed -> return ()

readReply :: Tracker -> IO (Maybe ByteString)
readReply (Tracker h) = do
    (d, status) <- readBulk h cmdInEndpt 255 cmdTimeout
    case status of
        TimedOut  -> error "Reply read timed out"
        Completed -> if BS.head d == 0x06
                       then return $ Just $ BS.tail d
                       else return Nothing
                       
readAck :: Tracker -> IO ()
readAck tracker = do
    a <- readReply tracker
    case a of
        Just _  -> return ()
        Nothing -> error "Ack expected"
