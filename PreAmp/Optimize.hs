{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}

module PreAmp.Optimize ( optimize
                       , GainOffset(..)
                       , gain, offset
                       ) where

import Prelude hiding (sequence)
import Data.Function
import Data.List (minimumBy)
import Data.Traversable (Traversable, sequenceA)
import Data.Foldable (Foldable)
import Control.Applicative
import Control.Concurrent (threadDelay)
import Control.Concurrent.STM
import Control.Monad.State hiding (sequence)
import Control.Monad.Trans.Either
import Control.Monad.Trans.Maybe
import Statistics.Sample (meanVariance)

import Control.Lens
import qualified Data.Vector as V

import PreAmp
import Tracker

-- | A lens that focuses on a PSD channel
type PsdLens = forall a. Lens' (PsdChannels a) a

data GainOffset a = GainOffset {_gain, _offset :: a}
                  deriving ( Show, Read, Ord, Eq, Functor, Foldable
                           , Traversable
                           )
makeLenses ''GainOffset

instance Applicative GainOffset where
    pure x = GainOffset x x
    GainOffset ga oa <*> GainOffset gb ob = GainOffset (ga gb) (oa ob)

readLastTChan :: TChan a -> STM a
readLastTChan tchan = do
    a <- readTChan tchan
    empty <- isEmptyTChan tchan
    if empty then return a
             else readLastTChan tchan

failE :: Monad m => EitherT String m a -> m a
failE m = either error id `liftM` runEitherT m

setGainOffset :: MonadIO m => PreAmp -> Channel -> GainOffset CodePoint
              -> TrackerT m ()
setGainOffset pa paCh go = do
    liftIO $ failE $ PreAmp.setGain pa paCh (go ^. gain)
    liftIO $ failE $ PreAmp.setOffset pa paCh (go ^. offset)

data MeanVar a = MeanVar {_mean, _var :: !a}
makeLenses ''MeanVar

sampleConfig :: (MonadIO m)
             => PreAmp -> Channel -> GainOffset CodePoint
             -> TrackerT m (PsdChannels (MeanVar Double))
sampleConfig pa paCh go = do
    setGainOffset pa paCh go
    queue <- getSensorQueue
    liftIO $ threadDelay 100
    s <- liftIO $ atomically $ readLastTChan queue
    let meanVar v = case meanVariance v of (mu,sig2) -> MeanVar mu sig2
    return $ fmap (\v->meanVar $ fmap fromIntegral
                               $ V.drop (V.length v - 10) v)
           $ sequenceA (V.map (^. (psd . _Unwrapped')) s)

sweepOffset :: (MonadIO m)
            => PreAmp -> PsdLens -> GainOffset CodePoint
            -> TrackerT m (Maybe (GainOffset CodePoint))
sweepOffset pa channel go = do
    let xs = map (\o->go & offset .~ o) [minBound..]
        paCh = PreAmp.channels ^. channel
    ys <- mapM (sampleConfig pa paCh) xs
    case minimumBy (compare `on` (\y->abs $ y^._2.channel.mean))
         $ filter (\y->y^._2.channel.var < 4000) (zip xs ys) of
        (x,y) | abs (y^.channel.mean) < 1000  -> return $ Just x
        _                                     -> return $ Nothing

sweepGain :: (MonadIO m)
          => PreAmp -> PsdLens -> TrackerT m (Maybe (GainOffset CodePoint))
sweepGain pa channel = runMaybeT $ step (GainOffset maxBound 0)
  where
    step :: (MonadIO m)
         => GainOffset CodePoint
         -> MaybeT (TrackerT m) (GainOffset CodePoint)
    step go
      | go ^. gain == 0  = return go
      | otherwise = do
          go' <- lift $ sweepOffset pa channel go
          case go' of
            Just x  -> return x
            Nothing -> step $ gain -~ 2 $ go

optimize :: (MonadIO m)
         => PreAmp -> Sample -> PsdLens
         -> TrackerT m (Maybe (GainOffset CodePoint))
optimize pa margin channel = do
    let paCh = PreAmp.channels ^. channel
    result <- sweepGain pa channel
    setGainOffset pa paCh $ maybe (GainOffset 0 0) id result
    return result
