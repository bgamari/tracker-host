{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveFoldable, DeriveFunctor, DeriveTraversable, TemplateHaskell #-}

module Tracker.Types ( Sample
                     , Fixed16
                     , Stage(..)
                     , mkStage
                     , StageAxis(..)
                     , stageAxes
                       -- * Modelling position sensitive photodiode
                     , SumDiff(SumDiff)
                     , sdSum, sdDiff
                     , Diode(Diode)
                     , anode, cathode
                     , Psd(Psd, unPsd)
                     , mkPsd
                     , sumDiffDiode
                       -- * Sensor inputs
                     , Sensors(Sensors)
                     , stage, psd
                       -- * Convenient re-exports
                     , MonadIO, liftIO
                     ) where
       
import Data.Int
import Data.Ratio
import Data.Foldable
import Data.Traversable
import Data.Fixed
import Data.Binary
import Data.Binary.Get (getWord32le)
import Data.Binary.Put (putWord32le)
import Control.Applicative       
import Data.Distributive (Distributive)
import Linear       
import Control.Lens
import Control.Monad.IO.Class

-- | An ADC sample       
type Sample = Int16

data F16 = F16
-- | A signed 16.16 fixed-point number
newtype Fixed16 = Fixed16 (Fixed F16)
                deriving (Show, Read, Eq, Ord, Fractional, Num, Real, RealFrac)
                
instance HasResolution F16 where
    resolution _ = 0x10000
    
instance Binary Fixed16 where
    get = Fixed16 . fromIntegral <$> getWord32le
    put (Fixed16 a) = putWord32le $ round $ 0x10000 * a
    
-- | The stage coordinate frame
newtype Stage a = Stage {unStage :: V3 a}
             deriving ( Show, Functor, Foldable, Traversable
                      , Applicative, Additive, Metric, Distributive
                      , R1, R2, R3)
        
mkStage :: a -> a -> a -> Stage a
mkStage x y z = Stage $ V3 x y z
    
data StageAxis = StageX | StageY | StageZ
               deriving (Show, Eq, Ord, Bounded, Enum)

stageAxes :: Stage StageAxis
stageAxes = mkStage StageX StageY StageZ

-- | A sum-difference sample
data SumDiff a = SumDiff { _sdSum, _sdDiff :: !a }
               deriving (Show, Functor, Foldable, Traversable)
makeLenses ''SumDiff

instance Applicative SumDiff where
    pure x = SumDiff x x
    SumDiff s d <*> SumDiff x y = SumDiff (s x) (d y)

instance Additive SumDiff where zero = pure 0

-- | Values associated with the anode and cathode of a diode
data Diode a = Diode { _anode, _cathode :: !a }
             deriving (Show, Functor, Foldable, Traversable)

makeLenses ''Diode
     
instance Applicative Diode where
    pure x = Diode x x
    Diode a c <*> Diode x y = Diode (a x) (c y)

instance Additive Diode where zero = pure 0

-- | The position-sensitive detector frame
newtype Psd a = Psd {unPsd :: V2 a}
              deriving ( Show, Functor, Foldable, Traversable, Applicative
                       , Additive, Metric, R1, R2)

mkPsd :: a -> a -> Psd a
mkPsd x y = Psd $ V2 x y

-- | Stage and PSD input
data Sensors a = Sensors { _stage :: !(Stage a)
                         , _psd   :: !(Psd (SumDiff a))
                         }
               deriving (Show, Functor)
makeLenses ''Sensors
           
instance Applicative Sensors where
    pure x = Sensors (pure x) (pure $ pure x)
    Sensors s1 p1 <*> Sensors s2 p2 = Sensors (s1 <*> s2) (fmap (<*>) p1 <*> p2)

sumDiffDiode :: Num a => Iso' (SumDiff a) (Diode a)
sumDiffDiode = iso to from
    where to (SumDiff sum diff) = Diode (sum - diff) (sum + diff)
          from (Diode an cat) = SumDiff (an - cat) (an + cat)
