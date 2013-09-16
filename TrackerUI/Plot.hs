{-# LANGUAGE RankNTypes #-}
                
module TrackerUI.Plot ( startPlot
                      , TrackerPlot
                      , setYSize
                      , setNPoints
                      ) where
      
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
decimate n = V.ifilter (\i _->i `mod` n == 0)

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

plotWorker :: TVar PlotConfig -> TChan (V.Vector (Sensors Int16)) -> IO ()
plotWorker configVar queue = do
    GLFW.setErrorCallback $ Just $ \err s->do error s
    result <- GLFW.init
    when (not result) $ error "Failed to initialize GLFW"

    psdPlot <- newPlot "Tracker PSD"
    stagePlot <- newPlot "Tracker Stage"
    let updatePlot :: PlotConfig -> Plot -> [Curve] -> IO ()
        updatePlot config plot cs = do
            let step = 1000
                (miny, maxy) = case config^.pcYSize of
                                 Just size -> let s = realToFrac size / 2 in (-s, s)
                                 Nothing   -> let ys = map (\c->c^.cPoints^.to VS.head._y.to realToFrac) cs
                                              in (minimum ys-2000, maximum ys+2000)
            setLimits plot $ Rect (V2 0 miny) (V2 (realToFrac $ config^.pcNPoints) maxy)
            updateCurves plot cs 

        go :: Sensors (VS.Vector Int16) -> IO ()
        go v = do
            config <- atomically $ readTVar configVar
            new <- atomically $ readTChan queue
            let v' = fmap (VS.take $ config^.pcNPoints)
                     $ (VS.++) <$> fmap VS.convert (T.sequenceA new) <*> v
            updatePlot config psdPlot $ psdCurves v'
            updatePlot config stagePlot $ stageCurves v'
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
        config <- liftIO $ newTVarIO $ PlotConfig { _pcYSize   = Nothing
                                                  , _pcNPoints = 10000
                                                  }
        worker <- forkOS $ plotWorker config queue
        return $ TrackerPlot worker config

setYSize :: TrackerPlot -> Maybe Int16 -> IO ()
setYSize = setConfig pcYSize

setNPoints :: TrackerPlot -> Int -> IO ()
setNPoints = setConfig pcNPoints
    
setConfig :: Lens' PlotConfig a -> TrackerPlot -> a -> IO ()
setConfig lens plot value =
    atomically $ modifyTVar (plot^.tpConfig) $ lens .~ value

