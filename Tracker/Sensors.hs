{-# LANGUAGE TemplateHaskell #-}

module Tracker.Sensors ( Sensors(..), stage, psd
                       , parseFrames
                       ) where

import Data.Int
import Data.Word
import Data.Binary.Get
import Data.Traversable
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Vector as V
import Control.Applicative
import Control.Lens
import Linear
import Tracker.Types

data Sensors a = Sensors { _stage :: !(Stage a)
                         , _psd   :: !(Psd (Diode a))
                         }
               deriving (Show)
makeLenses ''Sensors

getInt16le :: Get Int16
getInt16le = fromIntegral `fmap` getWord16le

parseFrames :: BS.ByteString -> V.Vector (Sensors Sample)
parseFrames a =
    runGet (V.replicateM (BS.length a `div` 32) frame) $ BSL.fromStrict a
  where frame = do stage <- sequenceA $ pure getInt16le :: Get (Stage Sample)
                   xDiff <- getInt16le
                   yDiff <- getInt16le
                   xSum  <- getInt16le
                   ySum  <- getInt16le
                   _ <- getInt16le
                   let sumDiff = Psd $ V2 (SumDiff xSum xDiff) (SumDiff ySum yDiff)
                   return $ Sensors stage (sumDiffToPsd sumDiff)

sumDiffToPsd :: Num a => Psd (SumDiff a) -> Psd (Diode a)
sumDiffToPsd = fmap diode
  where diode :: Num a => SumDiff a -> Diode a
        diode (SumDiff sum diff) = Diode (sum - diff) (sum + diff)
