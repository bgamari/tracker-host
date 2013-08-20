{-# LANGUAGE OverloadedStrings, PatternGuards, RankNTypes #-}

import qualified Data.Foldable as F
import Data.Maybe
import Data.List (isPrefixOf, stripPrefix, intercalate)
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import Control.Applicative
import Data.Int
import Numeric

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Vector as V

import Linear
import System.Console.Haskeline
import Data.Binary.Get
import Control.Lens hiding (setting)

import qualified Tracker as T
import Tracker (TrackerT, Stage(..), Psd(..), Sensors, Sample)
import TrackerUI.Types

unitStageGains :: Stage (Stage Int32)
unitStageGains = kronecker $ Stage $ V3 1 1 1

command :: String -> String -> String -> ([String] -> TrackerUI ()) -> Command
command name help args action = Cmd [name] (Just help) args (\a->action a >> return True)

exitCmd :: Command
exitCmd = Cmd ["exit"] (Just "Exit the program") "" $ const $ return False

helloCmd :: Command
helloCmd = command "hello" help ""
    $ const $ liftInputT $ outputStrLn "hello world!"
  where help = "Print hello world!"

setRawPositionCmd :: Command
setRawPositionCmd = command "set-pos" help "(X,Y,Z)" $ \args->
    case args of 
      x:_ | Just pos <- readMaybe x   -> liftTracker $ T.setRawPosition $ pos^.from (stage . v3Tuple)
      otherwise                       -> liftInputT $ outputStrLn "expected position"
  where help = "Set raw stage position"

centerCmd :: Command
centerCmd = command "center" help "" $ \args->
    liftTracker $ T.setRawPosition $ Stage $ V3 c c c
  where c = 0xffff `div` 2
        help = "Set stage at center position"

roughCalCmd :: Command
roughCalCmd = command "rough-cal" help "" $ \args->do
    rs <- use roughScan
    freq <- use roughScanFreq
    scan <- liftTracker $ T.roughScan freq rs
    lastRoughCal .= Just scan
  where help = "Perform rough calibration"

dumpRoughCmd :: Command
dumpRoughCmd = command "dump-rough" help "[FILENAME]" $ \args->do
    let fname = fromMaybe "rough-cal.txt" $ listToMaybe args
    scan <- use lastRoughCal
    case scan of
        Nothing -> liftInputT $ outputStrLn "No rough calibration done."
        Just s  -> do liftIO $ writeFile fname
                             $ unlines $ map showSensors $ V.toList s
                      liftInputT $ outputStrLn $ "Last rough calibration dumped to "++fname
  where help = "Dump last rough calibration"
        showSensors x = intercalate "\t" $ (F.toList $ fmap show $ x ^. T.stage) ++[""]++
                                           (F.concat $ fmap (F.toList . fmap show) $ x ^. T.psd)

fineCalCmd :: Command
fineCalCmd = command "fine-cal" help "" $ \args->do
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
readSensorsCmd = command "read-sensors" help "" $ \args->do
    liftTracker $ T.setAdcTriggerMode T.TriggerAuto
    let printSensors s = putStr $ unlines 
            [ "Stage = "++F.foldMap (flip showSInt "\t") (s^.T.stage)
            , "PSD   = "++F.concatMap (F.foldMap (flip showSInt "\t")) (s^.T.psd)
            ]
          where showSInt = showSigned showInt 0
    -- FIXME
    --liftTracker $ T.adcAcquire $ \s->liftIO (printSensors s) >> return False
    liftTracker $ T.setAdcTriggerMode T.TriggerOff
  where help = "Read sensors values"
  
resetCmd :: Command
resetCmd = command "reset" help "" $ \args->do
    liftTracker $ T.reset
    -- TODO: Quit or reconnect
  where help = "Perform hardware reset"
  
helpCmd :: Command
helpCmd = command "help" help "[CMD]" $ \args->
    let cmdFilter :: [Command] -> [Command]
        cmdFilter = case args of
                      [] -> id
                      _  -> filter (\c->(c^.cmdName) `isPrefixOf` args)
        cmds = cmdFilter commands
        formatCmd :: Command -> Maybe String
        formatCmd c = case c ^. cmdHelp of 
                         Just help -> Just $ take 40 (unwords (c^.cmdName)++" "++c^.cmdArgs++repeat ' ') ++ help
                         Nothing   -> Nothing
    in liftInputT $ outputStr
       $ case cmds of
           []  -> "No matching commands\n"
           _   -> unlines $ mapMaybe formatCmd cmds
  where help = "Display help message"

stage :: Iso' (Stage a) (V3 a)
stage = iso (\(Stage v)->v) Stage

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
                     Nothing    -> do liftInputT $ outputStrLn
                                                 $ "Invalid value: "++unwords args
                                      return True
        showValue value = liftInputT $ outputStrLn $ name++" = "++format value 

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
            (roughScan . T.scanSize . stage)
    , r3Setting "rough.center" "rough calibration field center in code-points"
            (roughScan . T.scanCenter . stage)
    , r3Setting "rough.points" "number of points in rough calibration scan"
            (roughScan . T.scanPoints . stage)
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
           , resetCmd
           , exitCmd
           , helpCmd
           ] ++ settings

prompt :: TrackerUI Bool
prompt = do
    input <- maybe ["exit"] words <$> liftInputT (getInputLine "> ")
    let cmds = filter (\c->(c^.cmdName) `isPrefixOf` input) commands
    case cmds of
      cmd:[]  -> do let Just rest = stripPrefix (cmd^.cmdName) input
                    cmd^.cmdAction $ rest
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
