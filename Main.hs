{-# LANGUAGE OverloadedStrings, PatternGuards, RankNTypes, RecordWildCards #-}

import qualified Data.Traversable as Traversable
import qualified Data.Foldable as F
import Data.Maybe
import Data.List (isPrefixOf, stripPrefix, intercalate)
import Control.Monad
import Control.Monad.Error.Class
import Control.Monad.State
import Control.Monad.Trans.Class
import Control.Monad.Trans.Either
import Control.Monad.IO.Class
import Control.Applicative
import Data.Int
import Numeric
import Control.Concurrent
import Control.Concurrent.STM

import Control.Monad (MonadPlus(..))
import qualified Control.Error.Safe as Safe
import Control.Error.Util
import Data.EitherR (fmapLT)

import System.IO
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Vector as V

import Linear
import System.Console.Haskeline
import Data.Binary.Get
import Control.Lens hiding (setting, Setting)

import qualified Tracker as T
import Tracker.Types
import TrackerUI.Types
import TrackerUI.Plot
import PreAmp       
import PreAmp.Optimize

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

centerCmd :: Command
centerCmd = command ["center"] help "" $ \args->
    liftTrackerE $ T.setRawPosition $ Stage $ V3 c c c
  where c = 0xffff `div` 2
        help = "Set stage at center position"

roughCalCmd :: Command
roughCalCmd = command ["rough-cal"] help "" $ \args->do
    rs <- use roughScan
    freq <- use roughScanFreq
    scan <- liftTrackerE $ T.roughScan freq rs
    lastRoughCal .= Just scan
  where help = "Perform rough calibration"

showSensors :: Show a => Sensors a -> String
showSensors x = intercalate "\t" $ (F.toList $ fmap show $ x ^. T.stage) ++[""]++
                                   (F.concat $ fmap (F.toList . fmap show) $ x ^. T.psd)

dumpRoughCmd :: Command
dumpRoughCmd = command ["dump-rough"] help "[FILENAME]" $ \args->do
    let fname = fromMaybe "rough-cal.txt" $ listToMaybe args
    s <- use lastRoughCal >>= tryJust "No rough calibration."
    liftIO $ writeFile fname $ unlines $ map showSensors $ V.toList s
    liftInputT $ outputStrLn $ "Last rough calibration dumped to "++fname
  where help = "Dump last rough calibration"

fineCalCmd :: Command
fineCalCmd = command ["fine-cal"] help "" $ \args->do
    fs <- use fineScan
    gains <- liftTrackerE $ T.fineCal fs
    feedbackGains .= gains
    let showF = showSigned (showEFloat (Just 2)) 1
    liftIO $ putStrLn "Feedback gains = "
    liftIO $ putStrLn $ unlines $ F.toList
           $ fmap (F.foldMap (\x->showF x "\t")) gains
    return ()
  where help = "Perform fine calibration"
  
readSensorsCmd :: Command
readSensorsCmd = command ["read-sensors"] help "" $ \args->do
    let showSensors s = unlines 
            [ "Stage = "++F.foldMap (flip showSInt "\t") (s^.T.stage)
            , "PSD   = "++F.concatMap (F.foldMap (flip showSInt "\t")) (s^.T.psd)
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
        plot' <- liftTracker startPlot
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

logger :: Handle -> Int -> TChan (V.Vector (Sensors Int16)) -> IO ()       
logger h decimation queue = forever $ do
    v <- atomically $ readTChan queue
    V.mapM_ (hPutStrLn h . showSensors)
        $ V.ifilter (\i _->i `mod` decimation == 0) v
    
logStartCmd :: Command
logStartCmd = command ["log","start"] help "FILE [DECIMATION]" $ \args->do
    use logThread >>= flip when (throwError "Already logging") . isJust
    fname <- liftEitherT $ Safe.tryAt "Expected filename" args 0
    let dec = fromMaybe 1 $ Safe.atZ args 1 >>= Safe.readZ
    h <- liftEitherT $ fmapLT show $ tryIO $ openFile fname WriteMode
    queue <- liftTracker T.getSensorQueue
    thread <- liftIO $ forkIO $ logger h dec queue
    logThread .= Just thread
    liftInputT $ outputStrLn $ "Logging sensor samples to "++fname
  where help = "Start logging sensor samples to given file"

logStopCmd :: Command
logStopCmd = command ["log","stop"] help "" $ \args->do
    thread <- use logThread
    case thread of 
        Nothing   -> throwError "Not currently logging"
        Just x    -> liftIO (killThread x) >> logThread .= Nothing
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
    liftTracker $ do
        optimize pa 1000 (_x . sdDiff)
        optimize pa 1000 (_y . sdDiff)
        optimize pa 1000 (_x . sdSum)
        optimize pa 1000 (_y . sdSum)
        return ()
  where help = "Automatically optimize pre amplifier gains and offsets"

preAmpCmds :: [Command]
preAmpCmds = concat [ cmd (_x.sdSum) "xsum"
                    , cmd (_x.sdDiff) "xdiff"
                    , cmd (_y.sdSum) "ysum"
                    , cmd (_y.sdDiff) "ydiff"
                    ]
             ++ [ openPreAmp, optimizePreAmp ]
  where cmd :: (forall a. Lens' (Psd (SumDiff a)) a) -> String -> [Command]
        cmd proj name = 
            [ Cmd ["set", "amp."++name++".gain"] 
                  (Just "Set pre-amplifier gain") "[GAIN]" $ \args -> do
                pa <- use preAmp >>= tryJust "No pre-amplifier open"
                gain <- tryHead "expected gain" args >>= tryRead "invalid gain"
                liftEitherT $ PreAmp.setGain pa ch $ fromIntegral gain
                return True
            , Cmd ["set", "amp."++name++".offset"] 
                  (Just "Set pre-amplifier offset") "[OFFSET]" $ \args -> do
                pa <- use preAmp >>= tryJust "No pre-amplifier open"
                offset <- tryHead "expected offset" args >>= tryRead "invalid offset"
                liftEitherT $ PreAmp.setOffset pa ch $ fromIntegral offset
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
        let test = fmap (view (psd . _x . sdSum)) samples
        traj <- uses (excitation . _x . excChanExcitation) T.excitationTrajectory
        liftIO $ print $ T.phaseAmp traj (fmap realToFrac test)
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
readParse' = Traversable.sequence . evalState go
  where go = Traversable.sequence (pure $ state parse)
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
    [ Setting name         (Just help) readParse show a (l . v3Tuple)
    ]++coreSettings name labels a l
  where labels = V3 "x" "y" "z"
  
coreSettings :: (Show a, Read a, Core f, F.Foldable f)
            => String -> f String
            -> Accessors TrackerUI b
            -> Lens' b (f a)
            -> [Setting]
coreSettings name labels a l =
    F.toList $ core $ \l'->
      let label = labels ^. l'
      in Setting (name++"."++label) Nothing readParse show a (l . l')
    
settings :: [Setting] 
settings = concat
    [ r3Setting "rough.size" "rough calibration field size in code-points"
            stateA (roughScan . T.scanSize . stageV3)
    , r3Setting "rough.center" "rough calibration field center in code-points"
            stateA (roughScan . T.scanCenter . stageV3)
    , r3Setting "rough.points" "number of points in rough calibration scan"
            stateA (roughScan . T.scanPoints . stageV3)
    , r3Setting "stage.output-gain.prop" "stage output proportional gain"
            (knobA T.outputGain) (incore propGain . stageV3 . mapping fixed16Double)
    , r3Setting "stage.output-gain.int" "stage output proportional gain"
            (knobA T.outputGain) (incore intGain . stageV3 . mapping fixed16Double)
    , r3Setting "stage.tau" "stage feedback integration time"
            (knobA T.outputTau) stageV3
    , r3Setting "stage.fb-gain.x" "stage feedback gain"
            (knobA T.stageGain) (_x . stageV3 . mapping fixed16Double)
    , r3Setting "stage.fb-gain.y" "stage feedback gain"
            (knobA T.stageGain) (_y . stageV3 . mapping fixed16Double)
    , r3Setting "stage.fb-gain.z" "stage feedback gain"
            (knobA T.stageGain) (_z . stageV3 . mapping fixed16Double)
    , r3Setting "stage.setpoint" "stage feedback setpoint"
            (knobA T.stageSetpoint) stageV3
    ] ++
    exciteSettings ++
    [ pureSetting "rough.freq"
            (Just "update frequency of rough calibration scan")
            readParse show roughScanFreq
    , Setting "stage.max-error" (Just "maximum tolerable error signal before killing feedback")
            readParse show (knobA T.maxError) id
    ]
    
fixed16Double :: Iso' Fixed16 Double
fixed16Double = iso realToFrac realToFrac

showCmd :: Command
showCmd = command ["show"] help "PATTERN" $ \args->do
    pattern <- tryHead "expected pattern" args
    let matching = filter (\(Setting {..})->pattern `isPrefixOf` sName)
                   $ filter (\(Setting {..})->isJust sHelp)
                   $ settings
    forM_ matching $ \(Setting {..})->do
        value <- sAccessors^.aGet
        liftInputT $ outputStrLn $ sName++" = "++views sLens sFormat value 
  where help = "Show values of settings matching pattern"

commands :: [Command]
commands = [ helloCmd
           , centerCmd
           , setRawPositionCmd
           , roughCalCmd
           , dumpRoughCmd
           , fineCalCmd
           , readSensorsCmd
           , logStartCmd
           , logStopCmd
           , sourceCmd
           , resetCmd
           , exitCmd
           , helpCmd
           , command ["adc", "start"] "Start ADC triggering" "" $ const
             $ liftTrackerE $ T.setAdcTriggerMode T.TriggerAuto
           , showCmd
           ] ++ concatMap settingCommands settings
             ++ preAmpCmds
             ++ plotCommands
             ++ exciteCmds
             ++ feedbackCmds

prompt :: TrackerUI Bool
prompt = do
    input <- maybe ["exit"] words <$> liftInputT (getInputLine "> ")
    runCommand input

runCommand :: [String] -> TrackerUI Bool
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

defaultStageGains :: Stage (Stage Fixed16)
defaultStageGains = kronecker $ Stage $ V3 1 1 1

defaultOutputGains :: Stage (PropInt Fixed16)
defaultOutputGains = pure (PropInt 1e-2 0)

main :: IO ()
main = either error (const $ return ()) =<< go
  where go = runTrackerUI commands $ do
          liftTrackerE $ do T.echo "Hello World!" >>= liftIO . print
                            T.setKnob T.stageGain defaultStageGains
                            T.setKnob T.outputGain defaultOutputGains
                            T.setFeedbackFreq 10000
                            T.setAdcFreq 10000
                            T.startAdcStream
                            T.setAdcTriggerMode T.TriggerAuto
          while $ prompt

while :: Monad m => m Bool -> m ()
while m = m >>= \a->when a (while m)
