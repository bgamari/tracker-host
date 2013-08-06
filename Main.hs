{-# LANGUAGE OverloadedStrings, PatternGuards #-}

import Data.Maybe
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
import Control.Lens       

import qualified Tracker as T
import Tracker (TrackerT, Stage(..), Psd(..), Sensors, Sample)
import TrackerUI.Types

roughScan :: T.RasterScan
roughScan =
    maybe (error "Invalid scan") id
    $ T.scanAround (pure 0x7fff) (pure 0x1000) (V3 20 20 2)

unitStageGains :: Stage (Stage Int32)
unitStageGains = Stage (Stage 1 0 0) (Stage 0 1 0) (Stage 0 0 1)

command :: String -> String -> String -> ([String] -> TrackerUI ()) -> Command
command name help args action = Cmd name help args (\a->action a >> return True)

exitCmd :: Command
exitCmd = Cmd "exit" "Exit the program" "" $ const $ return False

helloCmd :: Command
helloCmd = command "hello" help ""
    $ const $ liftInputT $ outputStrLn "hello world!"
  where help = "Print hello world!"

roughCalCmd :: Command
roughCalCmd = command "rough-cal" help "" $ \args->do
    scan <- liftTracker $ T.roughScan 1000 roughScan
    lastRoughCal .= Just scan
  where help = "Perform rough calibration"

dumpRoughCmd :: Command
dumpRoughCmd = command "dump-rough" help "[FILENAME]" $ \args->do
    let fname = fromMaybe "rough-cal.txt" $ listToMaybe args
    scan <- use lastRoughCal
    case scan of
        Nothing -> liftInputT $ outputStrLn "No rough calibration done."
        Just s  -> liftIO $ writeFile fname
                          $ unlines $ map (show . T.stage) $ V.toList s
  where help = "Dump last rough calibration"

helpCmd :: Command
helpCmd = command "help" help "[CMD]" $ \args->
    let cmdFilter = maybe id (\fc->filter (\c->c^.cmdName == fc)) $ listToMaybe args
        cmds = cmdFilter commands
        formatCmd :: Command -> String
        formatCmd c = take 40 (c^.cmdName++" "++c^.cmdArgs++repeat ' ')++c^.cmdHelp
    in liftInputT $ outputStr $ unlines $ map formatCmd cmds
  where help = "Display help message"

commands :: [Command]
commands = [ helloCmd
           , roughCalCmd
           , dumpRoughCmd
           , exitCmd
           , helpCmd
           ]

prompt :: TrackerUI Bool
prompt = do
    line <- liftInputT $ getInputLine "> "
    case maybe ["exit"] words line of
      cmd:rest | Just c <- lookupCmd cmd  -> c^.cmdAction $ rest
               | otherwise                -> do
                   liftInputT $ outputStrLn $ "Unknown command: "++cmd
                   return True
      _ | otherwise                       -> return True
  where lookupCmd :: String -> Maybe Command
        lookupCmd cmd = lookup cmd $ map (\c->(c^.cmdName, c)) commands

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
