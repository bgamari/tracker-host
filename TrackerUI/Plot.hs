{-# LANGUAGE RankNTypes #-}

module TrackerUI.Plot ( startPlot
                      , TrackerPlot
                      , setYSize
                      , setNPoints
                      , setDecimation
                      ) where

import Data.Foldable as F
import Data.Traversable as T
import Data.Int
import Control.Monad
import Control.Monad.Trans.Either
import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM
import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS
import Control.Lens

import Linear
import Tracker as T
import TrackerUI.Types
import TrackerUI.Queue
import TrackerUI.Plot.Types
import qualified Util.RingBuffer as RB

import Graphics.Rendering.GLPlot
import qualified Graphics.UI.GLFW as GLFW
import Graphics.Rendering.OpenGL.GL (GLfloat, Color4(..))

decimate :: Int -> V.Vector a -> V.Vector a
decimate n = V.ifilter (\i _->i `mod` n == 0)

hello :: Psd (SumDiff (a -> b)) -> Psd (SumDiff a) -> Psd (SumDiff b)
hello f a = fmap (<*>) f <*> a

psdCurveParams :: Psd (SumDiff CurveParams)
psdCurveParams = Psd $
    V2 (mkSumDiff (f "sum-x"  $ Color4 1 0 0 1)
                  (f "diff-x" $ Color4 0 1 1 1))
       (mkSumDiff (f "sum-y"  $ Color4 0 0 1 1)
                  (f "diff-y" $ Color4 1 0 1 1))
  where
    f name color = cName ?~ name
                 $ cColor .~ color
                 $ cStyle .~ Lines
                 $ defaultCurve

stageCurveParams :: Stage CurveParams
stageCurveParams =
    Stage $ V3 (f "x" $ Color4 0.8 0.5 0.3 1)
               (f "y" $ Color4 0.4 0.8 0.5 1)
               (f "z" $ Color4 0.4 0.5 0.8 1)
  where
    f name color = cName ?~ name
                 $ cColor .~ color
                 $ cStyle .~ Lines
                 $ defaultCurve

plotWorker :: TrackerQueue
           -> TVar PlotConfig
           -> TChan (V.Vector (Sensors Int16)) -> IO ()
plotWorker tq configVar queue = do
    ctx <- newContext
    let npts = 4000 -- TODO: Update ring size when needed
    psdPlot <- newPlot ctx "Tracker PSD"
    setLimits psdPlot $ Rect (V2 0 (-0x8010)) (V2 (realToFrac npts) (0x10010))
    psdCurves <- (traverse . traverse) (newCurve psdPlot) psdCurveParams
                 :: IO (Psd (SumDiff Curve))
    psdSetpointCurves <- (traverse . traverse) (newCurve psdPlot) psdCurveParams
                      :: IO (Psd (SumDiff Curve))
    stagePlot <- newPlot ctx "Tracker Stage"
    setLimits stagePlot $ Rect (V2 0 (-0x8010)) (V2 (realToFrac npts) (0x10010))
    stageCurves <- traverse (newCurve stagePlot) stageCurveParams
    let curves :: Sensors Curve
        curves = Sensors stageCurves psdCurves

    rings <- T.sequence $ pure $ RB.new npts
             :: IO (Sensors (RB.RingBuffer VS.Vector GLfloat))
    let updatePlot :: PlotConfig -> Plot -> [Curve] -> IO ()
        updatePlot config plot cs = do
            let (miny, maxy) =
                  case config^.pcYSize of
                    Just size -> let s = realToFrac size / 2 in (-s, s)
                    Nothing   -> (-0xffff, 0xffff)
                      --let ys = map (\c->c^.cPoints^.to VS.head._y.to realToFrac) cs
                      --in (F.minimum ys-2000, F.maximum ys+2000)
            setLimits plot $ Rect (V2 0 miny) (V2 (realToFrac $ config^.pcNPoints) maxy)

    let go t = do
        threadDelay $ 1000000 `div` 30
        let update :: Curve -> RB.RingBuffer VS.Vector GLfloat -> IO ()
            update curve rb =
                void $ RB.withItems rb $ setPoints curve . VS.imap (\i y->V2 (realToFrac i) y)
        F.sequence_ (update <$> curves <*> rings)
        go (t + 1e-5)
    drawer <- forkIO $ go 2

    void $ forkIO $ forever $ do
        threadDelay $ 1000000 `div` 2
        setpoints <- runTrackerQ tq $ runEitherT $ getKnob T.psdSetpoint
        case setpoints of
          Left err -> print $ "failed to fetch PSD setpoints: "++err
          Right s ->
            let go y curve = setPoints curve
                             $ VS.fromList
                               [ V2 0 (fromIntegral y)
                               , V2 (fromIntegral npts) (fromIntegral y)]
            in void $ traverse F.sequence_
               $ pure (pure go) `hello` s `hello` psdSetpointCurves

    void $ forever $ do
        config <- atomically $ readTVar configVar
        new <- atomically $ readTChan queue
        let d = config ^. pcDecimation
            new' :: Sensors (VS.Vector GLfloat)
            new' = fmap (VS.convert . V.map realToFrac)
                 $ T.sequenceA $ decimate d new
        F.sequence_ $ RB.concat <$> new' <*> rings

    void $ forever $ threadDelay 1000000 -- FIXME: Terminate
    GLFW.terminate
    return ()

startPlot :: TrackerUI TrackerPlot
startPlot = do
    queue <- liftTracker getSensorQueue
    tq <- getTrackerQueue
    liftIO $ do
        config <- newTVarIO
                  $ PlotConfig { _pcYSize   = Nothing
                               , _pcNPoints = 10000
                               , _pcDecimation = 10
                               }

        worker <- forkOS $ plotWorker tq config queue
        return $ TrackerPlot worker config

setYSize :: TrackerPlot -> Maybe Int16 -> IO ()
setYSize = setConfig pcYSize

setNPoints :: TrackerPlot -> Int -> IO ()
setNPoints = setConfig pcNPoints

setDecimation :: TrackerPlot -> Int -> IO ()
setDecimation = setConfig pcDecimation

setConfig :: Lens' PlotConfig a -> TrackerPlot -> a -> IO ()
setConfig l plot value =
    atomically $ modifyTVar (plot^.tpConfig) $ l .~ value
