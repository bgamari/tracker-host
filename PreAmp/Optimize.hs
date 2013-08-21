{-# LANGUAGE TemplateHaskell, RankNTypes #-}

module PreAmp.Optimize (optimize) where

import PreAmp
import Data.Function
import Data.List (minimumBy)
import Tracker
import Control.Lens
import Control.Concurrent.STM
import Control.Monad.State
import Control.Monad.Trans.Maybe
import qualified Data.Vector as V

-- | A lens that focuses on a PSD channel
type PsdLens = forall a. ReifiedLens' (Psd (SumDiff a)) a

readLastTChan :: TChan a -> STM a
readLastTChan tchan = do
    a <- readTChan tchan
    empty <- isEmptyTChan tchan
    if empty then return a
             else readLastTChan tchan

data GainOffset = GO { _gain, _offset :: CodePoint }
makeLenses ''GainOffset

sampleConfig :: (MonadIO m)
             => PreAmp -> Channel -> GainOffset -> TrackerT m (Psd (SumDiff Sample))
sampleConfig pa paCh go = do
    queue <- getSensorQueue
    liftIO $ PreAmp.setGain pa paCh (go ^. gain)
    liftIO $ PreAmp.setOffset pa paCh (go ^. offset)
    s <- liftIO $ atomically $ readLastTChan queue
    return $ s ^. to V.last . psd

sweepOffset :: (MonadIO m)
            => PreAmp -> PsdLens -> GainOffset -> TrackerT m (Maybe GainOffset)
sweepOffset pa channel go = do
    let xs = map (\o->go & offset .~ o) [minBound..]
        paCh = PreAmp.channels ^. reflectLens channel
    ys <- mapM (sampleConfig pa paCh) xs
    case minimumBy (compare `on` (\y->y^._2.reflectLens channel)) $ zip xs ys of
        (x,y) | y^.reflectLens channel < 1000  -> return $ Just x
        _                                      -> return $ Nothing

optimize :: (MonadIO m)
         => PreAmp -> Sample -> PsdLens -> TrackerT m ()
optimize pa margin channel = do
    let step :: MonadIO m => GainOffset -> MaybeT (TrackerT m) GainOffset
        step go = do
            go' <- MaybeT $ sweepOffset pa channel go
            next <- lift $ runMaybeT $ step $ gain +~ 10 $ go'
            case next of
              Just x  -> return x
              Nothing -> return go
    r <- runMaybeT $ step $ GO 0 0
    return ()


