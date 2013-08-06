{-# LANGUAGE OverloadedStrings, PatternGuards #-}

import Data.Maybe
import Control.Monad
import Control.Monad.IO.Class
import Control.Applicative
import Data.Binary.Get
import Data.Int       
       
import qualified Data.Vector as V
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
       
import qualified Tracker as T
import Tracker (Tracker, Stage(..), Psd(..))
import Linear
import System.Console.Haskeline
 
roughScan :: T.RasterScan
roughScan =
    maybe (error "Invalid scan") id
    $ T.scanAround (pure 0x7fff) (pure 0x1000) (V3 20 20 2)

unitStageGains :: Stage (Stage Int32)
unitStageGains = Stage (Stage 1 0 0) (Stage 0 1 0) (Stage 0 0 1)

main :: IO ()    
main = runInputT defaultSettings $ do
    t <- fromJust (error "Couldn't open Tracker") <$> liftIO T.open
    liftIO $ do T.echo t "Hello World!" >>= print
                T.setStageGains t unitStageGains
                T.setFeedbackFreq t 1000
                T.setAdcFreq t 5000
                --T.roughScan t 1000 roughScan >>= V.mapM_ (putStrLn . show . fst)
    while $ prompt t
    liftIO $ T.close t
    
while :: Monad m => m Bool -> m ()
while m = m >>= \a->when a (while m)

prompt :: Tracker -> InputT IO Bool
prompt t = do
    line <- getInputLine "> "
    case maybe ["exit"] words line of
      "exit":_                                      -> return False
      cmd:rest | Just action <- lookup cmd commands -> action rest >> return True
               | otherwise                          -> do outputStrLn $ "Unknown command: "++cmd
                                                          return True
      _                                             -> return True

commands :: [(String, [String] -> InputT IO ())]
commands = [ ("hello", const $ outputStrLn "hello") ]
