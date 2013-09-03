{-# LANGUAGE GeneralizedNewtypeDeriving, PatternGuards #-}

module PreAmp ( PreAmp
              , CodePoint
              , open
              , Channel
              , channels
              , setOffset
              , setGain
              ) where

import System.Hardware.Serialport
import System.IO
import Tracker.Types
import Linear
import Control.Applicative
import Control.Error.Util (tryIO)
import Control.Monad.Trans.Either
import Control.Monad.Trans
import Data.List (stripPrefix)
import Data.Word

newtype PreAmp = PreAmp Handle

newtype Channel = Ch Int
                deriving (Enum, Ord, Eq, Show)

newtype CodePoint = CP Word8
                  deriving (Enum, Num, Ord, Eq, Show)

instance Bounded CodePoint where
    minBound = CP 0
    maxBound = CP 255

channels = Psd $ V2 (SumDiff xSum xDiff) (SumDiff ySum yDiff)

xSum  = Ch 0
xDiff = Ch 1
ySum  = Ch 2
yDiff = Ch 3

write :: MonadIO m => PreAmp -> String -> m ()
write (PreAmp h) = liftIO . hPutStr h

readReply :: MonadIO m => PreAmp -> EitherT String m ()
readReply (PreAmp h) = EitherT $ do
    reply <- liftIO $ hGetLine h
    return $ case reply of
      s | Just err <- stripPrefix "!err" s  -> Left $ "Error occurred: "++err
        | Just _ <- stripPrefix "!ok" s     -> Right ()
        | otherwise                         -> Left $ "Unknown reponse: "++s

setOffset :: MonadIO m => PreAmp -> Channel -> CodePoint -> EitherT String m ()
setOffset pa (Ch n) (CP v) = do
    lift $ write pa $ "="++show n++"o="++show v++"\r\n"
    readReply pa

setGain :: MonadIO m => PreAmp -> Channel -> CodePoint -> EitherT String m ()
setGain pa (Ch n) (CP v) = do
    lift $ write pa $ "="++show n++"g"++show v++"\r\n"
    readReply pa
    

open :: MonadIO m => FilePath -> EitherT String m PreAmp
open port = do
    r <- liftIO $ runEitherT $ tryIO $ hOpenSerial port defaultSerialSettings { commSpeed = CS115200 }
    case r of
      Left err   -> left $ show err
      Right a    -> return $ PreAmp a
