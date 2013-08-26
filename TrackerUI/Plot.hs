module TrackerUI.Plot (startPlot) where
      
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
import TrackerUI.Plot.Types

import Graphics.Rendering.GLPlot
import qualified Graphics.UI.GLFW as GLFW
import Graphics.Rendering.OpenGL.GL (GLfloat, Color4(..))

npoints = 4000

fixPoints :: (VS.Storable a, Real a) => VS.Vector a -> VS.Vector (V2 GLfloat)
fixPoints = VS.imap (\x y->V2 (realToFrac x) (realToFrac y))

decimate :: Int -> V.Vector a -> V.Vector a
decimate n = fmap snd . V.filter (\(i,_)->i `mod` n == 0) . V.indexed

psdCurves :: Sensors (VS.Vector Int16) -> [Curve]
psdCurves pts =
    [   cColor  .~ Color4 1 0 0 0
      $ cPoints .~ fixPoints (pts ^. psd ^. _x ^. sdDiff) $ c
    ,   cColor  .~ Color4 0 1 1 0
      $ cPoints .~ fixPoints (pts ^. psd ^. _x ^. sdSum) $ c
    ,   cColor  .~ Color4 0 0 1 0
      $ cPoints .~ fixPoints (pts ^. psd ^. _y ^. sdDiff) $ c
    ,   cColor  .~ Color4 1 0 1 0
      $ cPoints .~ fixPoints (pts ^. psd ^. _y ^. sdSum) $ c
    ]
  where c = cStyle .~ Points $ defaultCurve
    
stageCurves :: Sensors (VS.Vector Int16) -> [Curve]
stageCurves pts =
    [   cColor  .~ Color4 0.8 0.5 0.3 0
      $ cPoints .~ fixPoints (pts ^. stage ^. _x) $ c
    ,   cColor  .~ Color4 0.4 0.8 0.5 0
      $ cPoints .~ fixPoints (pts ^. stage ^. _y) $ c
    ,   cColor  .~ Color4 0.4 0.5 0.8 0
      $ cPoints .~ fixPoints (pts ^. stage ^. _z) $ c
    ]
  where c = cStyle .~ Points $ defaultCurve

data UpDown = Up | Down

roundUD :: RealFrac a => UpDown -> a -> a -> a
roundUD ud k x 
  | b == 0    = k * realToFrac (a :: Int)
  | otherwise = k * realToFrac a + bump
  where (a, b) = properFraction (x / k)
        bump = case ud of
              Up     ->  1
              Down   -> -1

plotWorker :: Int -> TChan (V.Vector (Sensors Int16)) -> IO ()
plotWorker npoints queue = do
    GLFW.setErrorCallback $ Just $ \err s->do error s
    result <- GLFW.init
    when (not result) $ error "Failed to initialize GLFW"

    psdPlot <- newPlot "Tracker PSD"
    stagePlot <- newPlot "Tracker Stage"
    let updatePlot :: Plot -> [Curve] -> IO ()
        updatePlot plot cs = do
            let step = 1000
                (miny, maxy) = let ys = map (\c->c^.cPoints^.to VS.head._y.to realToFrac) cs
                               in (minimum ys, maximum ys)
                               --in (-0xffff, 0xffff)
                limits = Rect (V2 0 (miny-2000)) (V2 (realToFrac npoints) (maxy+2000))
            setLimits plot limits
            updateCurves plot cs 

        go :: Sensors (VS.Vector Int16) -> IO ()
        go v = do
            new <- atomically $ readTChan queue
            let v' = fmap (VS.take npoints)
                     $ (VS.++) <$> fmap VS.convert (T.sequenceA new) <*> v
            updatePlot psdPlot $ psdCurves v'
            updatePlot stagePlot $ stageCurves v'
            go v'
    listener <- forkIO $ go (pure VS.empty)
    mainLoop [psdPlot, stagePlot]
    killThread listener
    GLFW.terminate
    return ()

startPlot :: MonadIO m => TrackerT m TrackerPlot
startPlot = do
    queue <- getSensorQueue
    liftIO $ do
        worker <- forkOS $ plotWorker npoints queue
        ySize <- liftIO $ newTVarIO Nothing
        return $ TrackerPlot worker ySize

