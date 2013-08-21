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
import Control.Lens
import Tracker 
import TrackerUI.Types

npoints = 1000
       
points :: (Functor f, Foldable f) => f (V2 Float) -> Picture
points = pictures . toList . fmap (\(V2 x y)->translate x y $ circle 0.5)

plot :: (Functor f, Foldable f) => [(Color, f (V2 Float))] -> Picture       
plot curves =
    pictures $ [ color black $ line [(0xffff,0), (0,0), (0,0xffff)] ]
            ++ map (\(c,x)->color c $ points x) curves

fixPoints :: Real a => V.Vector a -> V.Vector (V2 Float)
fixPoints = fmap (\(x,y)->V2 (realToFrac x) (realToFrac y)) . V.indexed

decimate :: Int -> V.Vector a -> V.Vector a
decimate n = fmap snd . V.filter (\(i,_)->i `mod` n == 0) . V.indexed

update :: TVar (V.Vector (Sensors Int16)) -> Float -> IO Picture
update ptsVar t = do
    let n = 4
    pts <- atomically $ readTVar ptsVar
    return $ plot
        [ (blue,   fixPoints $ decimate n $ fmap (\a->a^.psd._x.sdDiff) pts)
        , (red,    fixPoints $ decimate n $ fmap (\a->a^.psd._x.sdSum) pts)
        , (green,  fixPoints $ decimate n $ fmap (\a->a^.psd._y.sdDiff) pts)
        , (yellow, fixPoints $ decimate n $ fmap (\a->a^.psd._y.sdSum) pts)
        ]

plotWorker :: Int -> TChan (V.Vector (Sensors Int16)) -> IO ()
plotWorker npoints queue = do
    points <- newTVarIO V.empty
    forkIO $ forever $ do
        v <- atomically $ readTChan queue
        atomically $ do old <- readTVar points
                        writeTVar points $ V.take npoints $ v V.++ old
    animateIO (InWindow "Tracker" (300,300) (400,400)) white (update points)

startPlot :: MonadIO m => TrackerT m ()
startPlot = do
    queue <- getSensorQueue
    liftIO $ forkIO $ plotWorker npoints queue
    return ()

