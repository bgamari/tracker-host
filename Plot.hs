module Plot (startPlot) where
      
import Data.Traversable as T
import Data.Int
import Control.Monad
import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM
import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS
import Control.Lens

import Linear
import Tracker 
import TrackerUI.Types

import Graphics.Rendering.GLPlot
import Graphics.UI.GLUT as GLUT

npoints = 4000

fixPoints :: (VS.Storable a, Real a) => VS.Vector a -> VS.Vector (V2 GLfloat)
fixPoints = VS.imap (\x y->V2 (realToFrac x) (realToFrac y))

decimate :: Int -> V.Vector a -> V.Vector a
decimate n = fmap snd . V.filter (\(i,_)->i `mod` n == 0) . V.indexed

curves :: Sensors (VS.Vector Int16) -> [Curve]
curves pts =
    [Curve (Color4 0 0 0 0) (fixPoints $ pts ^. psd ^. _x ^. sdDiff)]

plotWorker :: Int -> TChan (V.Vector (Sensors Int16)) -> IO ()
plotWorker npoints queue = do
    GLUT.getArgsAndInitialize
    actionOnWindowClose $= ContinueExectuion
    plot <- newPlot "Tracker"
    let go :: Sensors (VS.Vector Int16) -> IO ()
        go v = do
            new <- atomically $ readTChan queue
            let v' = fmap (VS.take npoints) $ (VS.++) <$> fmap VS.convert (T.sequenceA new) <*> v
            updateCurves plot $ curves v'
            go v'
    forkIO $ go (pure VS.empty)
    GLUT.mainLoop

startPlot :: MonadIO m => TrackerT m ()
startPlot = do
    queue <- getSensorQueue
    liftIO $ forkOS $ plotWorker npoints queue
    return ()

