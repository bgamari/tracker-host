{-# LANGUAGE OverloadedStrings, PatternGuards, RankNTypes #-}

import qualified Data.Foldable as F
import Data.Maybe
import Data.List (isPrefixOf, stripPrefix, intercalate)
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Either
import Control.Monad.IO.Class
import Control.Applicative
import Data.Int
import Numeric
import Control.Concurrent
import Control.Concurrent.STM

import Control.Monad (MonadPlus(..))
import Control.Error.Safe
import Control.Error.Util
import Data.EitherR (fmapLT)

import System.IO
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Vector as V

import Linear
import System.Console.Haskeline
import Data.Binary.Get
import Control.Lens hiding (setting)

import qualified Tracker as T
import Tracker.Types
import TrackerUI.Types
import Plot
import PreAmp       
import PreAmp.Optimize

unitStageGains :: Stage (Stage Int32)
unitStageGains = kronecker $ Stage $ V3 1 1 1

command :: [String] -> String -> String -> ([String] -> EitherT String TrackerUI ()) -> Command
command name help args action = Cmd name (Just help) args (\a->action a >> return True)

exitCmd :: Command
exitCmd = Cmd ["exit"] (Just "Exit the program") "" $ const $ return False

helloCmd :: Command
helloCmd = command ["hello"] help ""
    $ const $ lift $ liftInputT $ outputStrLn "hello world!"
  where help = "Print hello world!"

setRawPositionCmd :: Command
setRawPositionCmd = command ["set-pos"] help "(X,Y,Z)" $ \args->do
    pos <- tryHead "expected position" args >>= tryRead "invalid position"
    lift $ liftTracker $ T.setRawPosition $ pos^.from (stageV3 . v3Tuple)
  where help = "Set raw stage position"

centerCmd :: Command
centerCmd = command ["center"] help "" $ \args->
    lift $ liftTracker $ T.setRawPosition $ Stage $ V3 c c c
  where c = 0xffff `div` 2
        help = "Set stage at center position"

roughCalCmd :: Command
roughCalCmd = command ["rough-cal"] help "" $ \args->lift $ do
    rs <- use roughScan
    freq <- use roughScanFreq
    scan <- liftTracker $ T.roughScan freq rs
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
    lift $ liftInputT $ outputStrLn $ "Last rough calibration dumped to "++fname
  where help = "Dump last rough calibration"

fineCalCmd :: Command
fineCalCmd = command ["fine-cal"] help "" $ \args->lift $ do
    fs <- use fineScan
    gains <- liftTracker $ T.fineCal fs
    feedbackGains .= gains
    let showF = showSigned (showEFloat (Just 2)) 1
    liftIO $ putStrLn "Feedback gains = "
    liftIO $ putStrLn $ unlines $ F.toList
           $ fmap (F.foldMap (\x->showF x "\t")) gains
    return ()
  where help = "Perform fine calibration"
  
readSensorsCmd :: Command
readSensorsCmd = command ["read-sensors"] help "" $ \args->lift $ do
    liftTracker $ T.setAdcTriggerMode T.TriggerAuto
    let showSensors s = unlines 
            [ "Stage = "++F.foldMap (flip showSInt "\t") (s^.T.stage)
            , "PSD   = "++F.concatMap (F.foldMap (flip showSInt "\t")) (s^.T.psd)
            ]
          where showSInt = showSigned showInt 0
    s <- liftTracker $ T.getSensorQueue >>= liftIO . atomically . readTChan
    liftInputT $ outputStr $ showSensors $ V.head s
    liftTracker $ T.setAdcTriggerMode T.TriggerOff
  where help = "Read sensors values"
  
startPlotCmd :: Command
startPlotCmd = command ["start-plot"] help "" $ \args->lift $ liftTracker startPlot
  where help = "Start plot view"

indexZ :: MonadPlus m => Int -> [a] -> m a
indexZ 0 (x:xs) = return x
indexZ n []     = mzero
indexZ n (_:xs) = indexZ (n-1) xs

logger :: Handle -> Int -> TChan (V.Vector (Sensors Int16)) -> IO ()       
logger h decimation queue = forever $ do
    v <- atomically $ readTChan queue
    V.mapM_ (hPutStrLn h . showSensors)
        $ V.ifilter (\i _->i `mod` decimation == 0) v
    
logStartCmd :: Command
logStartCmd = command ["log","start"] help "FILE [DECIMATION]" $ \args->do
    use logThread >>= flip when (left "Already logging") . isJust
    fname <- indexZ 0 args
    let dec = fromMaybe 1 $ indexZ 1 args >>= readZ
    h <- fmapLT show $ tryIO $ openFile fname WriteMode
    queue <- lift $ liftTracker T.getSensorQueue
    thread <- liftIO $ forkIO $ logger h dec queue
    logThread .= Just thread
  where help = "Start logging sensor samples to given file"

logStopCmd :: Command
logStopCmd = command ["log","stop"] help "" $ \args->do
    thread <- use logThread
    case thread of 
        Nothing   -> left "Not currently logging"
        Just x    -> liftIO (killThread x) >> logThread .= Nothing
  where help = "Stop logging of sensor samples"
  
resetCmd :: Command
resetCmd = command ["reset"] help "" $ \args->do
    lift $ liftTracker $ T.reset
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
           []  -> left "No matching commands"
           _   -> lift $ liftInputT $ outputStr $ unlines $ mapMaybe formatCmd cmds
  where help = "Display help message"
  
openPreAmp :: Command
openPreAmp = command ["open-preamp"] help "DEVICE" $ \args->do
    device <- tryHead "expected device" args
    pa <- PreAmp.open device
    preAmp .= Just pa
  where help = "Open pre-amplifier device"

preAmpCmds :: [Command]
preAmpCmds = concat [ cmd (_x.sdSum) "xsum"
                    , cmd (_x.sdDiff) "xdiff"
                    , cmd (_y.sdSum) "ysum"
                    , cmd (_y.sdDiff) "ydiff"
                    ]
             ++ [ openPreAmp ]
  where cmd :: (forall a. Lens' (Psd (SumDiff a)) a) -> String -> [Command]
        cmd proj name = 
            [ Cmd ["set", "amp."++name++".gain"] 
                  (Just "Set pre-amplifier gain") "[GAIN]" $ \args -> do
                pa <- use preAmp >>= tryJust "No pre-amplifier open"
                gain <- tryHead "expected gain" args >>= tryRead "invalid gain"
                PreAmp.setGain pa ch $ fromIntegral gain
                return True
            , Cmd ["set", "amp."++name++".offset"] 
                  (Just "Set pre-amplifier offset") "[OFFSET]" $ \args -> do
                pa <- use preAmp >>= tryJust "No pre-amplifier open"
                offset <- tryHead "expected offset" args >>= tryRead "invalid offset"
                PreAmp.setOffset pa ch $ fromIntegral offset
                return True
            ]
          where ch = PreAmp.channels ^. proj

stageV3 :: Iso' (Stage a) (V3 a)
stageV3 = iso (\(Stage v)->v) Stage

v3Tuple :: Iso' (V3 a) (a,a,a)
v3Tuple = iso (\(V3 x y z)->(x,y,z)) (\(x,y,z)->V3 x y z)

readMaybe :: Read a => String -> Maybe a
readMaybe a =          
    case reads a of
      []           -> Nothing
      (value,_):_  -> Just value

readParse :: Read a => [String] -> Maybe a
readParse [] = Nothing
readParse (a:_) = readMaybe a

setting' :: String -> Maybe String -> ([String] -> Maybe a) -> (a -> String)
         -> Lens' TrackerState a -> [Command]
setting' name help parse format l = [getter, setter]
  where getter = Cmd ["get",name] (("Get "++) <$> help) "" $ \args->
                   use l >>= showValue >> return True
        setter = Cmd ["set",name] (("Set "++) <$> help) "VALUE" $ \args->
                   case parse args of
                     Just value -> do l .= value
                                      showValue value
                                      return True
                     Nothing    -> do lift $ liftInputT $ outputStrLn
                                           $ "Invalid value: "++unwords args
                                      return True
        showValue value = lift $ liftInputT $ outputStrLn $ name++" = "++format value 

setting :: String -> String -> ([String] -> Maybe a) -> (a -> String)
        -> Lens' TrackerState a -> [Command]
setting name help parse format l = setting' name (Just help) parse format l

r3Setting :: (Show a, Read a) => String -> String -> Lens' TrackerState (V3 a) -> [Command]
r3Setting name help l =
       setting' name         (Just help) readParse show (l . v3Tuple)
    ++ setting' (name++".x") Nothing     readParse show (l . _x)
    ++ setting' (name++".y") Nothing     readParse show (l . _y)
    ++ setting' (name++".z") Nothing     readParse show (l . _z)

settings :: [Command] 
settings = concat 
    [ r3Setting "rough.size" "rough calibration field size in code-points"
            (roughScan . T.scanSize . stageV3)
    , r3Setting "rough.center" "rough calibration field center in code-points"
            (roughScan . T.scanCenter . stageV3)
    , r3Setting "rough.points" "number of points in rough calibration scan"
            (roughScan . T.scanPoints . stageV3)
    , setting "rough.freq" "update frequency of rough calibration scan"
            readParse show roughScanFreq
    ]

commands :: [Command]
commands = [ helloCmd
           , centerCmd
           , setRawPositionCmd
           , roughCalCmd
           , dumpRoughCmd
           , fineCalCmd
           , readSensorsCmd
           , startPlotCmd
           , logStartCmd
           , logStopCmd
           , resetCmd
           , exitCmd
           , helpCmd
           ] ++ settings ++ preAmpCmds

prompt :: TrackerUI Bool
prompt = do
    input <- maybe ["exit"] words <$> liftInputT (getInputLine "> ")
    let cmds = filter (\c->(c^.cmdName) `isPrefixOf` input) commands
    case cmds of
      cmd:[]  -> do let Just rest = stripPrefix (cmd^.cmdName) input
                    r <- runEitherT $ cmd^.cmdAction $ rest
                    case r of
                      Left err   -> do liftInputT $ outputStrLn $ "error: "++err
                                       return True
                      Right a    -> return a
      []      -> do liftInputT $ outputStrLn $ "Unknown command: "++unwords input
                    return True
      _:_     -> do let matches = intercalate ", " $ map (\c->unwords $ c^.cmdName) cmds
                    liftInputT $ outputStrLn $ "Ambiguous command: matches "++matches
                    return True

main :: IO ()
main = either error (const $ return ()) =<< go
  where go = runTrackerUI commands $ do
          liftTracker $ do T.echo "Hello World!" >>= liftIO . print
                           T.setStageGains unitStageGains
                           T.setFeedbackFreq 1000
                           T.setAdcFreq 5000
                           T.startAdcStream
          while $ prompt

while :: Monad m => m Bool -> m ()
while m = m >>= \a->when a (while m)
