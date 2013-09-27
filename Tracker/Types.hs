{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveFoldable, DeriveFunctor, DeriveTraversable, TemplateHaskell #-}

module Tracker.Types ( Sample
                     , Fixed16
                     , getFixed16le, putFixed16le
                     , Stage(..)
                     , mkStage
                     , StageAxis(..)
                     , stageAxes
                       -- * Modelling position sensitive photodiode
                     , SumDiff, mkSumDiff
                     , sdSum, sdDiff
                     , Psd(Psd, unPsd)
                     , mkPsd
                       -- * Sensor inputs
                     , Sensors(Sensors)
                     , stage, psd
                       -- * Feedback
                     , PropInt(PropInt)
                     , propGain, intGain
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
type Fixed16 = Fixed F16
                
instance HasResolution F16 where
    resolution _ = 0x10000
    
getFixed16le :: Get Fixed16
getFixed16le =
    f . fromIntegral <$> getWord32le
  where f :: Fixed16 -> Fixed16
        f x = x / realToFrac (resolution (undefined :: Fixed F16))
{-# INLINE getFixed16le #-}

putFixed16le :: Fixed16 -> Put
putFixed16le a =
    putWord32le $ round $ realToFrac (resolution (0::Fixed F16)) * a
{-# INLINE putFixed16le #-}
    
-- | The stage coordinate frame
newtype Stage a = Stage {unStage :: V3 a}
             deriving ( Show, Functor, Foldable, Traversable
                      , Applicative, Additive, Metric, Distributive
                      , R1, R2, R3, Core, Num, Fractional
                      )
        
mkStage :: a -> a -> a -> Stage a
mkStage x y z = Stage $ V3 x y z
{-# INLINE mkStage #-}

data StageAxis = StageX | StageY | StageZ
               deriving (Show, Eq, Ord, Bounded, Enum)

stageAxes :: Stage StageAxis
stageAxes = mkStage StageX StageY StageZ
{-# INLINE stageAxes #-}

-- | A sum-difference sample
newtype SumDiff a = SumDiff {unSumDiff :: V2 a}
                  deriving ( Show, Functor, Foldable, Traversable, Applicative
                           , Additive, Metric, R1, R2, Core, Num, Fractional
                           )

mkSumDiff :: a -> a -> SumDiff a
mkSumDiff s d = SumDiff $ V2 s d

sumDiffIso :: Iso' (SumDiff a) (V2 a)
sumDiffIso = iso unSumDiff SumDiff

sdSum, sdDiff :: Lens' (SumDiff a) a
sdSum = sumDiffIso . _x
sdDiff = sumDiffIso . _y

-- | The position-sensitive detector frame
newtype Psd a = Psd {unPsd :: V2 a}
              deriving ( Show, Functor, Foldable, Traversable, Applicative
                       , Additive, Metric, R1, R2, Core, Num, Fractional
                       )

mkPsd :: a -> a -> Psd a
mkPsd x y = Psd $ V2 x y
{-# INLINE mkPsd #-}

-- | Stage and PSD input
data Sensors a = Sensors { _stage :: !(Stage a)
                         , _psd   :: !(Psd (SumDiff a))
                         }
               deriving (Show, Functor)
makeLenses ''Sensors
           
instance Applicative Sensors where
    pure x = Sensors (pure x) (pure $ pure x)
    Sensors s1 p1 <*> Sensors s2 p2 = Sensors (s1 <*> s2) (fmap (<*>) p1 <*> p2)

data PropInt a = PropInt { _propGain, _intGain :: a }
               deriving (Show, Read, Eq, Ord, Functor, Traversable, Foldable)
makeLenses ''PropInt

instance Applicative PropInt where
    pure x = PropInt x x
    PropInt a b <*> PropInt x y = PropInt (a x) (b y)           
