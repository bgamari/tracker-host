{-# LANGUAGE OverloadedStrings, PatternGuards, RankNTypes #-}

import Data.Maybe
import Data.List (isPrefixOf, stripPrefix, intercalate)
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import Control.Applicative
import Data.Int

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
unitStageGains = Stage (Stage 1 0 0) (Stage 0 1 0) (Stage 0 0 1)

command :: String -> String -> String -> ([String] -> TrackerUI ()) -> Command
command name help args action = Cmd [name] help args (\a->action a >> return True)
        
readParse :: Read a => [String] -> Maybe a
readParse [] = Nothing
readParse (a:_) =
    case reads a of
      []           -> Nothing
      (value,_):_  -> Just value

setting :: String -> String -> ([String] -> Maybe a) -> (a -> String)
        -> Lens' TrackerState a -> [Command]
setting name help parse format l = [getter, setter]
  where getter = Cmd ["get",name] ("Get "++help) "" $ \args->do
                   use l >>= showValue >> return True
        setter = Cmd ["set",name] ("Set "++help) "VALUE" $ \args->do
                   case parse args of
                     Just value -> do l .= value
                                      showValue value
                                      return True
                     Nothing    -> do liftInputT $ outputStrLn
                                                 $ "Invalid value: "++unwords args
                                      return True
        showValue value = liftInputT $ outputStrLn $ name++" = "++format value 

stageTuple :: Iso' (Stage a) (a,a,a)
stageTuple = iso (\(Stage x y z)->(x,y,z)) (\(x,y,z)->Stage x y z)

roughScanSizeCmd :: [Command]
roughScanSizeCmd =
    setting "rough.size" "rough calibration field size in points"
            readParse show (roughScan . T.scanSize . stageTuple)   

exitCmd :: Command
exitCmd = Cmd ["exit"] "Exit the program" "" $ const $ return False

helloCmd :: Command
helloCmd = command "hello" help ""
    $ const $ liftInputT $ outputStrLn "hello world!"
  where help = "Print hello world!"

roughCalCmd :: Command
roughCalCmd = command "rough-cal" help "" $ \args->do
    rs <- use roughScan
    scan <- liftTracker $ T.roughScan 1000 rs
    lastRoughCal .= Just scan
  where help = "Perform rough calibration"

dumpRoughCmd :: Command
dumpRoughCmd = command "dump-rough" help "[FILENAME]" $ \args->do
    let fname = fromMaybe "rough-cal.txt" $ listToMaybe args
    scan <- use lastRoughCal
    case scan of
        Nothing -> liftInputT $ outputStrLn "No rough calibration done."
        Just s  -> liftIO $ writeFile fname
                          $ unlines $ map (show . (^. T.stage)) $ V.toList s
  where help = "Dump last rough calibration"

helpCmd :: Command
helpCmd = command "help" help "[CMD]" $ \args->
    let cmdFilter :: [Command] -> [Command]
        cmdFilter = case args of
                      [] -> id
                      _  -> filter (\c->(c^.cmdName) `isPrefixOf` args)
        cmds = cmdFilter commands
        formatCmd :: Command -> String
        formatCmd c = take 40 (unwords (c^.cmdName)++" "++c^.cmdArgs++repeat ' ')
                      ++ c^.cmdHelp
    in liftInputT $ outputStr
       $ case cmds of
           []  -> "No matching commands\n"
           _   -> unlines $ map formatCmd cmds
  where help = "Display help message"

commands :: [Command]
commands = [ helloCmd
           , roughCalCmd
           , dumpRoughCmd
           , exitCmd
           , helpCmd
           ]
           ++ roughScanSizeCmd

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
  where go = runTrackerUI $ do
          liftTracker $ do T.echo "Hello World!" >>= liftIO . print
                           T.setStageGains unitStageGains
                           T.setFeedbackFreq 1000
                           T.setAdcFreq 5000
          while $ prompt

while :: Monad m => m Bool -> m ()
while m = m >>= \a->when a (while m)
