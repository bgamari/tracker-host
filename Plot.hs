module Plot (startPlot) where
      
import Data.Foldable
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Animate
import Linear
import Data.Int
import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import qualified Data.Vector as V
import Tracker 
import TrackerUI.Types

points :: (Functor f, Foldable f) => f (V2 Float) -> Picture
points = Pictures . toList . fmap (\(V2 x y)->translate x y $ circle 0.5)

update :: TVar (V.Vector (Sensors Int16)) -> Float -> IO Picture
update pts t =
    return $ color black $ points [V2 x (10*sin (0.1*x + 3*t)) | x <- [0..4000]]

plotWorker :: TChan (V.Vector (Sensors Int16)) -> IO ()
plotWorker queue = do
    points <- newTVarIO V.empty
    forkIO $ forever $ do
        v <- atomically $ readTChan queue
        atomically $ do old <- readTVar points
                        writeTVar points $ V.take 500 $ v V.++ old
    animateIO (InWindow "Tracker" (300,300) (400,400)) white (update points)

startPlot :: MonadIO m => TrackerT m ()
startPlot = do
    queue <- getSensorQueue
    liftIO $ forkIO $ plotWorker queue
    return ()

