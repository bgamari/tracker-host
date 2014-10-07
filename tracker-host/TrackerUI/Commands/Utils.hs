{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TrackerUI.Commands.Utils where

import Data.Foldable as F

import Control.Lens hiding (setting, Setting)
import Data.Functor.Rep
import Linear

import qualified Data.ByteString.Lazy as BSL
import Data.Char (ord)

import qualified Control.Error.Safe as Safe
import Control.Monad.Error.Class

import qualified Data.Csv as Csv

import qualified Tracker as T
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

-- | Return the stage to center
center :: TrackerUI ()
center = use centerPos >>= liftTrackerE . T.setRawPosition . fmap fromIntegral

stageV3 :: Iso' (T.Stage a) (V3 a)
stageV3 = iso (\(T.Stage v)->v) T.Stage

v3Tuple :: Iso' (V3 a) (a,a,a)
v3Tuple = iso (\(V3 x y z)->(x,y,z)) (\(x,y,z)->V3 x y z)

readParse :: Read a => [String] -> Maybe a
readParse [] = Nothing
readParse (a:_) = Safe.readZ a

setting :: (Show a, Read a)
        => String -> String
        -> Accessors TrackerUI b -> Lens' b a -> Setting
setting name help a l =
    Setting name (Just help) readParse show a l

r3Setting :: (Show a, Read a)
          => String -> String
          -> Accessors TrackerUI b -> Lens' b (V3 a) -> [Setting]
r3Setting name help a l =
    setting name help a (l . v3Tuple) : repSettings name labels a l
  where
    labels = V3 "x" "y" "z"

repSettings :: forall a f b. (Show a, Read a, Representable f, Foldable f, Rep f ~ E f)
            => String -> f String
            -> Accessors TrackerUI b
            -> Lens' b (f a)
            -> [Setting]
repSettings name labels a l =
    let f :: f Setting
        f = tabulate $ \l'->
              let label = labels ^. el l'
              in Setting (name++"."++label) Nothing readParse show a (l . el l')
    in toList f
