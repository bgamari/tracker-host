{-# LANGUAGE RankNTypes #-}

module Trap.Sensor (fromChannel, decimate) where

import Control.Monad.IO.Class
import Control.Concurrent.STM
import Pipes
import qualified Data.Vector.Generic as VG

fromChannel :: MonadIO m => TChan a -> Producer' a m r
fromChannel chan = go
  where
    go = do
      x <- liftIO $ atomically $ readTChan chan
      yield x
      go

decimate :: (VG.Vector v a, Monad m) => Int -> Pipe (v a) a m r
decimate dec = go VG.empty dec
  where
    go xs n
      | VG.null xs = do
        xs' <- await
        go xs' n
      | VG.length xs < n = do
        go VG.empty (n - VG.length xs)
      | otherwise = do
        yield (xs VG.! n)
        go (VG.drop n xs) dec
