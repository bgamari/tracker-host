module TrackerUI.Commands.Utils where

import qualified Data.ByteString.Lazy as BSL
import Data.Char (ord)

import qualified Control.Error.Safe as Safe
import Control.Monad.Error.Class

import qualified Data.Csv as Csv
import TrackerUI.Types

writeTsv :: Csv.ToRecord a => FilePath -> [a] -> IO ()
writeTsv fname = BSL.writeFile fname . Csv.encodeWith opts
    where
      opts = Csv.defaultEncodeOptions { Csv.encDelimiter=fromIntegral $ ord '\t' }

tryHead :: String -> [a] -> TrackerUI a
tryHead e []    = throwError e
tryHead _ (x:_) = return x

tryRead :: Read a => String -> String -> TrackerUI a
tryRead e = maybe (throwError e) return . Safe.readZ

tryJust :: String -> Maybe a -> TrackerUI a
tryJust e Nothing  = throwError e
tryJust _ (Just a) = return a

command :: [String] -> String -> String -> ([String] -> TrackerUI ()) -> Command
command name help args action = Cmd name (Just help) args (\a->action a >> return True)
