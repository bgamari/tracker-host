{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TrackerUI.Commands (commands, runCommand) where

import Prelude hiding (concatMap, concat, sequence, mapM_)

import Data.Maybe
import Data.Monoid ((<>))
import Data.Char (ord)
import Data.Int
import Numeric
import Data.Foldable as F
import Data.Traversable
import Data.List (isPrefixOf, stripPrefix, intercalate)
import Control.Applicative
import Control.Monad.State hiding (sequence, forM_, mapM_)
import Control.Monad.Error.Class
import Control.Monad.Trans.Maybe
import System.IO

import Control.Concurrent
import Control.Concurrent.STM

import Data.Functor.Rep
import Control.Lens hiding (setting, Setting)
import Linear

import Control.Error.Util
import Data.EitherR (fmapLT)
import qualified Control.Error.Safe as Safe
import System.Console.Haskeline

import qualified Data.ByteString.Lazy as BSL
import qualified Data.Vector as V
import qualified Data.Csv as Csv

import qualified Tracker as T
import Tracker.Types
import Tracker.RoughCal.Model as Model
import TrackerUI.Types
import TrackerUI.Plot
import PreAmp
import PreAmp.Optimize as PreAmp

psdChannelNames :: PsdChannels String
psdChannelNames =
    PsdChans $ mkPsd (mkSumDiff "x.sum" "x.diff")
                     (mkSumDiff "y.sum" "y.diff")

tabulatePsdChannels :: ((forall a. Lens' (PsdChannels a) a) -> b)
                    -> PsdChannels b
tabulatePsdChannels f = PsdChans $ mkPsd
    (mkSumDiff (f (_Wrapped' . _x . sdSum)) (f (_Wrapped' . _x . sdDiff)))
    (mkSumDiff (f (_Wrapped' . _y . sdSum)) (f (_Wrapped' . _y . sdDiff)))

writeTsv :: Csv.ToRecord a => FilePath -> [a] -> IO ()
writeTsv fname = BSL.writeFile fname . Csv.encodeWith opts
    where opts = Csv.defaultEncodeOptions { Csv.encDelimiter=fromIntegral $ ord '\t' }

tryHead :: String -> [a] -> TrackerUI a
tryHead err []    = throwError err
tryHead _   (x:_) = return x

tryRead :: Read a => String -> String -> TrackerUI a
tryRead err = maybe (throwError err) return . Safe.readZ

tryJust :: String -> Maybe a -> TrackerUI a
tryJust err Nothing  = throwError err
tryJust _   (Just a) = return a

command :: [String] -> String -> String -> ([String] -> TrackerUI ()) -> Command
command name help args action = Cmd name (Just help) args (\a->action a >> return True)

exitCmd :: Command
exitCmd = Cmd ["exit"] (Just "Exit the program") "" $ const $ return False

helloCmd :: Command
helloCmd = command ["hello"] help ""
    $ const $ liftInputT $ outputStrLn "hello world!"
  where help = "Print hello world!"

setRawPositionCmd :: Command
setRawPositionCmd = command ["set-pos"] help "(X,Y,Z)" $ \args->do
    pos <- tryHead "expected position" args >>= tryRead "invalid position"
    liftTrackerE $ T.setRawPosition $ pos^.from (stageV3 . v3Tuple)
  where help = "Set raw stage position"

-- | Return the stage to center
center :: TrackerUI ()
center = use centerPos >>= liftTrackerE . T.setRawPosition . fmap fromIntegral

centerCmd :: Command
centerCmd = command ["center"] help "" $ \args->center
  where help = "Set stage at center position"

scanCmd :: Command
scanCmd = command ["scan"] help "[file]" $ \args -> do
    fname <- tryHead "expected file name" args
    rs <- use roughScan
    freq <- use roughScanFreq
    scan <- liftTrackerE $ T.roughScan freq rs
    liftIO $ writeTsv fname $ V.toList scan
    liftInputT $ outputStrLn $ "Scan dumped to "++fname
    center
  where help = "Perform a scan and dump to file (uses rough scan parameters)"

roughScanCmd :: Command
roughScanCmd = command ["rough", "scan"] help "" $ \args->do
    rs <- uses roughScan $ (T.scanSize . _z .~ 0)
                         . (T.scanPoints . _z .~ 1)
    freq <- use roughScanFreq
    scan <- liftTrackerE $ T.roughScan freq rs
    lastRoughScan .= Just scan
    center
  where help = "Perform rough scan"

roughCenterCmd :: Command
roughCenterCmd = command ["rough", "center"] help "" $ \args->do
    scan <- use lastRoughScan >>= tryJust "No last rough calibration"
    let c = T.roughCenter scan
    liftInputT $ outputStrLn $ show c
    roughScan . T.scanCenter .= fmap round c
    fineScan . T.fineScanCenter .= fmap round c
    centerPos .= fmap round c
    center
  where help = "Find center of particle from XY rough scan"

roughZScanCmd :: Command
roughZScanCmd = command ["rough", "zscan"] help "" $ \args->do
    scan <- use lastRoughScan >>= tryJust "No last rough calibration"
    rs <- uses roughScan $ (T.scanSize . _y .~ 0)
                         . (T.scanPoints . _y .~ 1)
    freq <- use roughScanFreq
    zScan <- liftTrackerE $ T.roughScan freq rs
    lastRoughZScan .= Just zScan
    center
  where help = "Perform rough Z scan"

showMatrix :: (Show a, Functor f, Foldable f, Functor g, Foldable g)
           => f (g a) -> String
showMatrix xs = unlines $ F.toList $ fmap (F.fold . fmap pad) xs'
  where xs' = fmap (fmap show) xs
        maxLength = F.maximum $ fmap (F.maximum . fmap length) xs'
        pad x = take (maxLength+4) $ x++repeat ' '

roughFitCmd :: Command
roughFitCmd = command ["rough", "fit"] help "" $ \args->do
    scan <- use lastRoughScan >>= tryJust "No rough calibration"
    let samples = V.map (\s->(s^.stage._xy, s^.psd._x.sdDiff))
                  $ V.map (fmap realToFrac) scan
        m0 = Model.initialModel samples
        m = head $ drop 10 $ Model.fit samples m0
        center = Model.modelCenter m
        --gains = Model.modelToGains center m
    liftIO $ print m0
    liftIO $ print m
    --liftIO $ print gains
    --when ("gains" `elem` args)
    --  $ liftTrackerE $ T.setKnob T.psdGains gains
  where help = "Perform fit on rough calibration"

showSensors :: Show a => Sensors a -> String
showSensors x = intercalate "\t" $ (F.toList $ fmap show $ x ^. T.stage) ++[""]++
                                   (F.concat $ fmap (F.toList . fmap show) $ x ^. T.psd)

setPsdSetpointCmd :: Command
setPsdSetpointCmd = command ["set-psd-setpoint"] help "" $ \args->do
    liftTrackerE $ do
      s <- lift T.getSensorQueue >>= liftIO . atomically . readTChan
      let sample = V.head s ^. T.psd
      T.setKnob T.psdSetpoint (sample & mapped . mapped %~ fromIntegral)
  where help = "Set PSD feedback setpoint to current sensor values"

dumpRoughCmd :: Command
dumpRoughCmd = command ["rough", "dump"] help "[FILENAME]" $ \args->do
    let fname = fromMaybe "rough-cal.txt" $ listToMaybe args
    s <- use lastRoughScan >>= tryJust "No rough calibration."
    liftIO $ writeTsv fname $ V.toList s
    liftInputT $ outputStrLn $ "Last rough calibration dumped to "++fname
  where help = "Dump last rough calibration"

dumpZRoughCmd :: Command
dumpZRoughCmd = command ["rough", "zdump"] help "[FILENAME]" $ \args->do
    let fname = fromMaybe "rough-zcal.txt" $ listToMaybe args
    s <- use lastRoughZScan >>= tryJust "No rough Z calibration."
    liftIO $ writeTsv fname $ V.toList s
    liftInputT $ outputStrLn $ "Last rough calibration dumped to "++fname
  where help = "Dump last rough Z calibration"

fineScanCmd :: Command
fineScanCmd = command ["fine", "scan"] help "" $ \args->do
    points <- use fineScan >>= liftTrackerE . T.fineScan
    lastFineScan ?= points
  where help = "Perform fine calibration scan"

fineCalCmd :: Command
fineCalCmd = command ["fine", "cal"] help "" $ \args->do
    s <- use fineScale
    points <- use lastFineScan >>= tryJust "No fine calibration scan"
    let (psdSetpt, gains) = T.fineCal points
        gains' = over (mapped . mapped . mapped) (realToFrac . (*s)) gains
    liftTrackerE $ do
        T.setKnob T.psdGains gains'
        T.setKnob T.psdSetpoint $ over (mapped . mapped) round psdSetpt
    let showF = showSigned (showEFloat (Just 2)) 1
    liftIO $ putStrLn "Feedback gains = "
    liftIO $ putStrLn $ unlines
           $ fmap (F.foldMap (\x->shows x "\t"))
           $ concat $ F.toList $ fmap F.toList gains'
    liftIO $ putStrLn "Feedback setpoint = "
    liftIO $ putStrLn $ concat $ fmap (F.foldMap (\x->shows x "\t")) $ F.toList psdSetpt
  where help = "Perform fine calibration regression"

fineDumpCmd :: Command
fineDumpCmd = command ["fine", "dump"] help "" $ \args->do
    let fname = fromMaybe "fine-cal.txt" $ listToMaybe args
    s <- use lastFineScan >>= tryJust "No fine calibration."
    liftIO $ writeTsv fname $ V.toList s
    liftInputT $ outputStrLn $ "Last fine calibration dumped to "++fname
  where help = "Dump fine calibration points"

readSensorsCmd :: Command
readSensorsCmd = command ["sensors", "read"] help "" $ \args->do
    let showSensors s = unlines
            [ "Stage = "++F.foldMap (flip showSInt "\t") (s^.T.stage)
            , "PSD   = "++intercalate "\t" [ "x-sum="++showSInt (s^.T.psd._x.sdSum) ""
                                           , "x-diff="++showSInt (s^.T.psd._x.sdDiff) ""
                                           , "y-sum="++showSInt (s^.T.psd._y.sdSum) ""
                                           , "y-diff="++showSInt (s^.T.psd._y.sdDiff) ""
                                           ]
            ]
          where showSInt = showSigned showInt 0
    s <- liftTracker $ T.getSensorQueue >>= liftIO . atomically . readTChan
    liftInputT $ outputStr $ showSensors $ V.head s
  where help = "Read sensors values"

startPlotCmd :: Command
startPlotCmd = command ["plot", "start"] help "" $ \args->do
    plot <- use trackerPlot
    case plot of
      Nothing -> do
        plot' <- startPlot
        trackerPlot .= Just plot'
      Just _  -> do
        throwError $ "Plot already running"
  where help = "Start plot view"

setPlotNPointsCmd :: Command
setPlotNPointsCmd = command ["set", "plot.npoints"] help "" $ \args->do
    plot <- use trackerPlot >>= tryJust "No plot"
    tryHead "Expected number of points" args >>= tryRead "Invalid number of points" >>= liftIO . setNPoints plot
  where help = "Set number of points in plot"

setYSizeCmd :: Command
setYSizeCmd = command ["set", "plot.ysize"] help "" $ \args->do
    plot <- use trackerPlot >>= tryJust "No plot"
    size <- case args of
              []  -> return Nothing
              x:_ -> Just <$> tryRead "Invalid size" x
    liftIO $ setYSize plot size
  where help = "Set Y extent"

plotCommands :: [Command]
plotCommands = [ startPlotCmd
               , setPlotNPointsCmd
               , setYSizeCmd
               ]

logger :: Handle -> Int -> TChan (V.Vector (Sensors Int16)) -> TVar Int -> IO ()
logger h decimation queue countVar = forever $ do
    v <- atomically $ readTChan queue
    let decimated = V.ifilter (\i _->i `mod` decimation == 0) v
    atomically $ modifyTVar countVar (+V.length decimated)
    V.mapM_ (hPutStrLn h . showSensors) decimated

killLogger :: Handle -> ThreadId -> TVar Int -> TrackerUI ()
killLogger h thread countVar = do
    liftIO $ killThread thread
    liftIO $ hClose h
    count <- liftIO $ atomically $ readTVar countVar
    liftInputT $ outputStrLn $ "Logged "++show count++" samples"

logStartCmd :: Command
logStartCmd = command ["log","start"] help "FILE [DECIMATION]" $ \args->do
    use stopLogger >>= flip when (throwError "Already logging") . isJust
    fname <- liftEitherT $ Safe.tryAt "Expected filename" args 0
    let dec = fromMaybe 1 $ Safe.atZ args 1 >>= Safe.readZ
    h <- liftEitherT $ fmapLT show $ tryIO $ openFile fname WriteMode
    queue <- liftTracker T.getSensorQueue
    countVar <- liftIO $ newTVarIO 0
    thread <- liftIO $ forkIO $ logger h dec queue countVar
    stopLogger .= Just (killLogger h thread countVar)
    liftInputT $ outputStrLn $ "Logging sensor samples to "++fname
  where help = "Start logging sensor samples to given file"

logStopCmd :: Command
logStopCmd = command ["log","stop"] help "" $ \args->do
    stop <- use stopLogger
    case stop of
        Nothing   -> throwError "Not currently logging"
        Just x    -> x >> stopLogger .= Nothing
  where help = "Stop logging of sensor samples"

sourceCmd :: Command
sourceCmd = command ["source"] help "FILE" $ \args->do
    fname <- liftEitherT $ Safe.tryAt "Expected filename" args 0
    cmds <- liftEitherT $ fmapLT show $ tryIO $ readFile fname
    mapM_ (runCommand . words) $ lines cmds
  where help = "Execute commands from the given file"

resetCmd :: Command
resetCmd = command ["reset"] help "" $ \args->do
    liftTrackerE T.reset
    -- TODO: Quit or reconnect
  where help = "Perform hardware reset"

eventCountersCmd :: Command
eventCountersCmd = command ["event-counters"] help "" $ \args->do
    liftTrackerE T.getEventCounters >>= liftInputT . outputStrLn . show
  where help = "Show event counters"

helpCmd :: Command
helpCmd = command ["help"] help "[CMD]" $ \args->
    let cmdFilter :: [Command] -> [Command]
        cmdFilter = case args of
                      [] -> id
                      _  -> filter (\c->(c^.cmdName) `isPrefixOf` args)
        cmds = cmdFilter commands
        formatCmd :: Command -> Maybe String
        formatCmd c = case c ^. cmdHelp of
                         Just help -> Just $ take 40 (unwords (c^.cmdName)++" "++c^.cmdArgs++repeat ' ') ++ help
                         Nothing   -> Nothing
    in case cmds of
           []  -> throwError "No matching commands"
           _   -> liftInputT $ outputStr $ unlines $ mapMaybe formatCmd cmds
  where help = "Display help message"

openPreAmp :: Command
openPreAmp = command ["preamp", "open"] help "DEVICE" $ \args->do
    device <- tryHead "expected device" args
    pa <- liftEitherT $ PreAmp.open device
    preAmp .= Just pa
  where help = "Open pre-amplifier device"

optimizePreAmp :: Command
optimizePreAmp = command ["preamp", "optimize"] help "" $ \args->do
    pa <- tryJust "No pre-amplifier open" =<< use preAmp
    maxVar <- use preAmpMaxSigma2
    let channel :: (forall a. Lens' (PsdChannels a) a)
                -> TrackerUI (GainOffset CodePoint)
        channel l = do
            liftIO $ putStr $ "Optimizing "++views l show names++": "
            res <- liftTrackerE $ noteT "Failed to optimize"
                                $ MaybeT $ optimize pa l maxVar
            liftIO $ print res
            return res

        names :: PsdChannels String
        names = PsdChans $ mkPsd (mkSumDiff "sumX" "diffX")
                                 (mkSumDiff "sumY" "diffY")

        actions :: PsdChannels (TrackerUI (GainOffset CodePoint))
        actions =
          PsdChans
          $ Psd $ V2 (mkSumDiff (channel $ _Wrapping' PsdChans . _x . sdSum)
                                (channel $ _Wrapping' PsdChans . _x . sdDiff))
                     (mkSumDiff (channel $ _Wrapping' PsdChans . _y . sdSum)
                                (channel $ _Wrapping' PsdChans . _y . sdDiff))
    values <- sequence actions
    preAmpValues .= values
  where help = "Automatically optimize pre amplifier gains and offsets"

resetPreAmp :: Command
resetPreAmp = command ["preamp", "reset"] help "" $ \args->do
    pa <- tryJust "No pre-amplifier open" =<< use preAmp
    let reset ch = PreAmp.setOffset pa ch 0 >> PreAmp.setGain pa ch 0
    void $ liftEitherT $ traverse reset PreAmp.channels
  where help = "Reset pre-amplifier gains and offsets to zero"

preAmpCmds :: [Command]
preAmpCmds = concat [ cmd (_Wrapping' PsdChans . _x . sdSum) "xsum"
                    , cmd (_Wrapping' PsdChans . _x . sdDiff) "xdiff"
                    , cmd (_Wrapping' PsdChans . _y . sdSum) "ysum"
                    , cmd (_Wrapping' PsdChans . _y . sdDiff) "ydiff"
                    ]
             ++ [ openPreAmp, optimizePreAmp, resetPreAmp ]
  where cmd :: (forall a. Lens' (PsdChannels a) a) -> String -> [Command]
        cmd proj name =
            [ Cmd ["set", "amp."++name++".gain"]
                  (Just "Set pre-amplifier gain") "[GAIN]" $ \args -> do
                pa <- use preAmp >>= tryJust "No pre-amplifier open"
                gain <- tryHead "expected gain" args >>= tryRead "invalid gain"
                liftEitherT $ PreAmp.setGain pa ch $ fromIntegral gain
                preAmpValues . proj . PreAmp.gain .= fromIntegral gain
                return True
            , Cmd ["get", "amp."++name++".gain"]
                  (Just "Get pre-amplifier gain") "" $ \args -> do
                uses (preAmpValues . proj . gain) print
                return True
            , Cmd ["set", "amp."++name++".offset"]
                  (Just "Set pre-amplifier offset") "[OFFSET]" $ \args -> do
                pa <- use preAmp >>= tryJust "No pre-amplifier open"
                offset <- tryHead "expected offset" args >>= tryRead "invalid offset"
                liftEitherT $ PreAmp.setOffset pa ch $ fromIntegral offset
                preAmpValues . proj . PreAmp.offset .= fromIntegral offset
                return True
            , Cmd ["get", "amp."++name++".offset"]
                  (Just "Get pre-amplifier offset") "" $ \args -> do
                uses (preAmpValues . proj . PreAmp.offset) print
                return True
            ]
          where ch = PreAmp.channels ^. proj

exciteCmds :: [Command]
exciteCmds =
    [ command ["excite", "start"]
      "Start excitation" "" $ \args->
        use excitation >>= liftTrackerE . T.configureExcitation . fmap maybeExciteChannel
    , command ["excite", "stop"]
      "Stop excitation" "" $ \args->do
        liftTrackerE $ T.configureExcitation (pure Nothing)
    , command ["excite", "cal"]
      "Run calibration" "" $ \args->do
        samples <- use corrPoints >>= liftTracker . fetchPoints
        let test = fmap (view (psd . _x . sdDiff)) samples
        decimation <- liftTrackerE $ T.getKnob T.adcDecimation
        decimatedExc <- uses (excitation . _x . excChanExcitation)
                             (T.excitePeriod %~ (/ realToFrac decimation) . realToFrac)
        phaseAmp <- liftTracker $ T.phaseAmp decimatedExc (fmap realToFrac test)
        liftIO $ print phaseAmp
        liftIO $ withFile "r.txt" WriteMode $ \h->
            V.mapM_ (hPutStrLn h . showSensors) samples
    ]

exciteSettings :: [Setting]
exciteSettings =
    concat [ f "x" _x, f "y" _y, f "z" _z ]++
    [ Setting "excite.corr-points" (Just "number of points to use in correlation")
              readParse show stateA corrPoints
    ]
  where f :: String
          -> Lens' (Stage ExciteChannel) ExciteChannel
          -> [Setting]
        f n l = [ Setting ("excite."++n++".period")
                          (Just "excitation period")
                          readParse show stateA
                          (excitation . l . excChanExcitation . T.excitePeriod)
                , Setting ("excite."++n++".amp")
                          (Just "excitation amplitude")
                          readParse show stateA
                          (excitation . l . excChanExcitation . T.exciteAmp)
                , Setting ("excite."++n++".enabled")
                          (Just "excitation enabled")
                          readParse show stateA
                          (excitation . l . excChanEnabled)
                ]

fetchPoints :: MonadIO m => Int -> T.TrackerT m (V.Vector (Sensors Sample))
fetchPoints n = do
    queue <- T.getSensorQueue
    let go v | V.length v >= n  = return $ V.take n v
             | otherwise        = do v' <- liftIO $ atomically $ readTChan queue
                                     go (v V.++ v')
    go V.empty

feedbackCmds :: [Command]
feedbackCmds =
    [ command ["feedback", "stop"] "Stop feedback" "" $ \args->
        liftTrackerE $ T.setKnob T.feedbackMode T.NoFeedback
    , command ["feedback", "psd"] "Start PSD feedback" "" $ \args->
        liftTrackerE $ T.setKnob T.feedbackMode T.PsdFeedback
    , command ["feedback", "stage"] "Start stage feedback" "" $ \args->
        liftTrackerE $ T.setKnob T.feedbackMode T.StageFeedback
    , command ["feedback", "search"] "Start particle search feedback" "" $ \args->
        liftTrackerE $ T.setKnob T.feedbackMode T.SearchFeedback
    , command ["feedback", "coarse"] "Start coarse feedback" "" $ \args->
        liftTrackerE $ T.setKnob T.feedbackMode T.CoarseFeedback
    , command ["feedback", "status"] "Show feedback status" "" $ \args->
        liftTrackerE (T.getKnob T.feedbackMode) >>= liftInputT . outputStrLn . show
    ]

stageV3 :: Iso' (Stage a) (V3 a)
stageV3 = iso (\(Stage v)->v) Stage

v3Tuple :: Iso' (V3 a) (a,a,a)
v3Tuple = iso (\(V3 x y z)->(x,y,z)) (\(x,y,z)->V3 x y z)

readParse :: Read a => [String] -> Maybe a
readParse [] = Nothing
readParse (a:_) = Safe.readZ a

readParse' :: (Read a, Traversable f, Applicative f)
           => [String] -> Either String (f a)
readParse' = sequence . evalState go
  where go = sequence (pure $ state parse)
        --parse :: [String] -> (Either String a, [String])
        parse []     = (Left "Too few arguments",               [])
        parse (x:xs) = (note "Unable to parse" $ Safe.readZ x,  xs)

settingCommands :: Setting -> [Command]
settingCommands (Setting {..}) = [getter, setter]
  where get = sAccessors ^. aGet
        put = sAccessors ^. aPut
        getter = Cmd ["get",sName] (("Get "++) <$> sHelp) "" $ \args->
                   get >>= showValue . view sLens >> return True
        setter = Cmd ["set",sName] (("Set "++) <$> sHelp) "VALUE" $ \args->
                   case sParse args of
                     Just value -> do get >>= put . (sLens .~ value)
                                      showValue value
                                      return True
                     Nothing    -> do liftInputT $ outputStrLn
                                           $ "Invalid value: "++unwords args
                                      return True
        showValue value = liftInputT $ outputStrLn $ sName++" = "++sFormat value

r3Setting :: (Show a, Read a)
          => String -> String
          -> Accessors TrackerUI b -> Lens' b (V3 a) -> [Setting]
r3Setting name help a l =
    [ Setting name (Just help) readParse show a (l . v3Tuple)
    ]++coreSettings name labels a l
  where labels = V3 "x" "y" "z"

coreSettings :: forall a f b. (Show a, Read a, Representable f, Foldable f, Rep f ~ E f)
             => String -> f String
             -> Accessors TrackerUI b
             -> Lens' b (f a)
             -> [Setting]
coreSettings name labels a l =
    let f :: f Setting
        f = tabulate $ \l'->
              let label = labels ^. el l'
              in Setting (name++"."++label) Nothing readParse show a (l . el l')
    in toList f

roughCalSettings :: [Setting]
roughCalSettings = concat
    [ r3Setting "rough.size" "rough calibration field size in code-points"
            stateA (roughScan . T.scanSize . stageV3)
    , r3Setting "rough.center" "rough calibration field center in code-points"
            stateA (roughScan . T.scanCenter . stageV3)
    , r3Setting "rough.points" "number of points in rough calibration scan"
            stateA (roughScan . T.scanPoints . stageV3)
    , [pureSetting "rough.freq" (Just "update frequency of rough calibration scan")
            readParse show roughScanFreq]
    ]

fineCalSettings :: [Setting]
fineCalSettings = concat
    [ [ pureSetting "fine.points" (Just "number of points in fine calibration")
            readParse show (fineScan . T.fineScanPoints)]
    , r3Setting "fine.range" "size of fine calibration in code points"
            stateA (fineScan . T.fineScanRange . stageV3)
    , [pureSetting "fine.gain-scale" (Just "factor to scale result of regression by to get feedback gains")
            readParse show fineScale]
    ]

stageSettings :: [Setting]
stageSettings = concat
    [ r3Setting "stage.output-gain.prop" "stage output proportional gain"
            (knobA T.outputGain) (column propGain . stageV3 . mapping realDouble)
    , r3Setting "stage.output-gain.int" "stage output proportional gain"
            (knobA T.outputGain) (column intGain . stageV3 . mapping realDouble)
    , r3Setting "stage.tau" "stage feedback integration time"
            (knobA T.outputTau) stageV3
    , r3Setting "stage.fb-gain.x" "stage feedback gain"
            (knobA T.stageGain) (_x . stageV3 . mapping realDouble)
    , r3Setting "stage.fb-gain.y" "stage feedback gain"
            (knobA T.stageGain) (_y . stageV3 . mapping realDouble)
    , r3Setting "stage.fb-gain.z" "stage feedback gain"
            (knobA T.stageGain) (_z . stageV3 . mapping realDouble)
    , r3Setting "stage.setpoint" "stage feedback setpoint"
            (knobA T.stageSetpoint) stageV3
    , [Setting "stage.max-error" (Just "maximum tolerable error signal before killing feedback")
            readParse show (knobA T.maxError) id]
    ]

psdSettings :: [Setting]
psdSettings = concat $ toList $ tabulatePsdChannels channel
  where
    channel :: (forall a. Lens' (PsdChannels a) a) -> [Setting]
    channel l = concat
        [ r3Setting ("psd.fb-gain."<>name) "PSD feedback gain"
                    (knobA T.psdGains)
                    (_Unwrapped' . l . stageV3 . mapping realDouble)
        , [Setting ("psd.fb-setpoint."<>name)
                   (Just "PSD feedback setpoint")
                   readParse show
                   (knobA T.psdSetpoint) (_Unwrapped' . l)]
        ]
      where
        name = psdChannelNames ^. l

searchSettings :: [Setting]
searchSettings = concat
    [ r3Setting "search.step" "Search feedback step size"
                (knobA T.searchStep) stageV3
    , [Setting "search.obj-thresh"
              (Just "Search objective function threshold")
              readParse show (knobA T.searchObjThresh) id]
    , toList $ tabulatePsdChannels gain
    ]
  where
    gain :: (forall a. Lens' (PsdChannels a) a) -> Setting
    gain l =
        Setting ("search.gains."<>name) (Just "Search objective gain")
                readParse show (knobA T.searchObjGains) l
      where
        name = psdChannelNames ^. l

coarseFbSettings :: [Setting]
coarseFbSettings = concat $ toList $ tabulatePsdChannels go
  where
    go :: (forall a. Lens' (PsdChannels a) a) -> [Setting]
    go l = concat 
        [ r3Setting ("coarse."<>name<>".high.step")
                    "Coarse feedback high step"
                    (knobA T.coarseFbParams)
                    (l . T.coarseStepHigh)
        , r3Setting ("coarse."<>name<>".low.step")
                    "Coarse feedback low step"
                    (knobA T.coarseFbParams)
                    (l . T.coarseStepLow)
        , [Setting "coarse.tol" (Just "Coarse feedback low step")
                  readParse show
                  (knobA T.coarseFbParams)
                  (l . T.coarseTolerance)]
        ]
      where
        name = psdChannelNames ^. l

settings :: [Setting]
settings = concat
    [ roughCalSettings, fineCalSettings, stageSettings, exciteSettings
    , psdSettings, searchSettings, coarseFbSettings
    , [Setting "decimation" (Just "decimation factor of samples")
            readParse show (knobA T.adcDecimation) id]
    , [Setting "preamp.optimize.maxVar" (Just "Maximum variance allowed in PSD signal")
            readParse show stateA preAmpMaxSigma2]
    ]

realDouble :: RealFrac a => Iso' a Double
realDouble = iso realToFrac realToFrac

showCmd :: Command
showCmd = command ["show"] help "PATTERN" $ \args->do
    let matching = filter (\(Setting {..})->isJust sHelp) $
          case listToMaybe args of
            Just pattern -> filter (\(Setting {..})->pattern `isPrefixOf` sName) settings
            Nothing      -> settings
    forM_ matching $ \(Setting {..})->do
        value <- sAccessors^.aGet
        liftInputT $ outputStrLn $ sName++" = "++views sLens sFormat value
  where help = "Show values of settings matching pattern"

commands :: [Command]
commands = [ helloCmd
           , centerCmd
           , setRawPositionCmd
           , scanCmd
           , roughScanCmd
           , roughCenterCmd
           , roughZScanCmd
           , roughFitCmd
           , setPsdSetpointCmd
           , dumpRoughCmd
           , dumpZRoughCmd
           , fineScanCmd
           , fineCalCmd
           , fineDumpCmd
           , readSensorsCmd
           , logStartCmd
           , logStopCmd
           , sourceCmd
           , resetCmd
           , eventCountersCmd
           , exitCmd
           , helpCmd
           , command ["adc", "start"] "Start ADC triggering" "" $ const
             $ liftTrackerE $ T.setKnob T.adcTriggerMode T.TriggerAuto
           , showCmd
           ] ++ concatMap settingCommands settings
             ++ preAmpCmds
             ++ plotCommands
             ++ exciteCmds
             ++ feedbackCmds

runCommand :: [String] -> TrackerUI Bool
runCommand [] = return True
runCommand input = do
    let cmds = filter (\c->(c^.cmdName) `isPrefixOf` input) commands
    case cmds of
      cmd:[]  -> do let Just rest = stripPrefix (cmd^.cmdName) input
                    catchError (cmd^.cmdAction $ rest) $ \err->do
                        liftInputT $ outputStrLn $ "error: "++err
                        return True
      []      -> do liftInputT $ outputStrLn $ "Unknown command: "++unwords input
                    return True
      _:_     -> do let matches = intercalate ", " $ map (\c->unwords $ c^.cmdName) cmds
                    liftInputT $ outputStrLn $ "Ambiguous command: matches "++matches
                    return True
