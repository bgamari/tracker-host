{-# LANGUAGE RankNTypes #-}

module TrackerUI.Plot ( startPlot
                      , TrackerPlot
                      , setYSize
                      , setNPoints
                      ) where

import Data.Foldable as F
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
import qualified Util.RingBuffer as RB

import Graphics.Rendering.GLPlot
import qualified Graphics.UI.GLFW as GLFW
import Graphics.Rendering.OpenGL.GL (GLfloat, Color4(..))

fixPoints :: (VS.Storable a, Real a) => VS.Vector a -> VS.Vector (V2 GLfloat)
fixPoints = VS.imap (\x y->V2 (realToFrac x) (realToFrac y))

decimate :: Int -> V.Vector a -> V.Vector a
decimate n = V.ifilter (\i _->i `mod` n == 0)

psdColors :: Psd (SumDiff (Color4 GLfloat))
psdColors =
    Psd $ V2 (mkSumDiff (Color4 1 0 0 0) (Color4 0 1 1 0))
             (mkSumDiff (Color4 0 0 1 0) (Color4 1 0 1 0))

stageColors :: Stage (Color4 GLfloat)
stageColors =
    Stage $ V3 (Color4 0.8 0.5 0.3 0)
               (Color4 0.4 0.8 0.5 0)
               (Color4 0.4 0.5 0.8 0)

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

    ctx <- newContext
    psdPlot <- newPlot ctx "Tracker PSD"
    psdCurves <- traverseOf (traverse . traverse) (\c->newCurve psdPlot $ defaultCurve & cColor .~ c) psdColors
                 :: IO (Psd (SumDiff Curve))
    stagePlot <- newPlot ctx "Tracker Stage"
    stageCurves <- T.traverse (\c->newCurve stagePlot $ defaultCurve & cColor .~ c) stageColors
    let curves :: Sensors Curve
        curves = Sensors stageCurves psdCurves

    -- TODO: Update ring size when needed
    rings <- T.sequence $ pure $ RB.new 30000
             :: IO (Sensors (RB.RingBuffer VS.Vector GLfloat))
    let updatePlot :: PlotConfig -> Plot -> [Curve] -> IO ()
        updatePlot config plot cs = do
            let step = 1000
                (miny, maxy) =
                  case config^.pcYSize of
                    Just size -> let s = realToFrac size / 2 in (-s, s)
                    Nothing   ->
                      --let ys = map (\c->c^.cPoints^.to VS.head._y.to realToFrac) cs
                      let ys = [-10000, 10000] -- FIXME
                      in (F.minimum ys-2000, F.maximum ys+2000)
            setLimits plot $ Rect (V2 0 miny) (V2 (realToFrac $ config^.pcNPoints) maxy)

    listener <- forkIO $ forever $ do
        config <- atomically $ readTVar configVar
        new <- atomically $ readTChan queue
        let d = config ^. pcDecimation
            new' :: Sensors (VS.Vector GLfloat)
            new' = fmap (VS.convert . V.map realToFrac)
                 $ T.sequenceA $ decimate d new
        F.sequence_ $ RB.concat <$> new' <*> rings

    drawer <- forkIO $ forever $ do
        threadDelay $ 1000000 `div` 30
        let update :: Curve -> RB.RingBuffer VS.Vector GLfloat
                   -> IO ()
            update curve rb =
                RB.withItems rb
                $ setPoints curve . VS.imap (\i y->V2 (realToFrac i) y)
        T.sequence (update <$> curves <*> rings)
        -- FIXME set limits

    forever $ threadDelay 1000000 -- FIXME: Terminate
    killThread listener
    GLFW.terminate
    return ()

startPlot :: MonadIO m => TrackerT m TrackerPlot
startPlot = do
    queue <- getSensorQueue
    liftIO $ do
        config <- liftIO $ newTVarIO
                  $ PlotConfig { _pcYSize   = Nothing
                               , _pcNPoints = 10000
                               , _pcDecimation = 10
                               }
        worker <- forkOS $ plotWorker config queue
        return $ TrackerPlot worker config

setYSize :: TrackerPlot -> Maybe Int16 -> IO ()
setYSize = setConfig pcYSize

setNPoints :: TrackerPlot -> Int -> IO ()
setNPoints = setConfig pcNPoints

setDecimation :: TrackerPlot -> Int -> IO ()
setDecimation = setConfig pcDecimation

setConfig :: Lens' PlotConfig a -> TrackerPlot -> a -> IO ()
setConfig lens plot value =
    atomically $ modifyTVar (plot^.tpConfig) $ lens .~ value
