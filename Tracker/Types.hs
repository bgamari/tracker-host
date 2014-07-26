{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}

module Tracker.Types ( -- * Fixed point numbers
                       Sample
                     , module Tracker.Types.Fixed
                       -- * Stage coordinate frame
                     , Stage(..)
                     , mkStage
                     , StageAxis(..)
                     , stageAxes
                       -- * Position sensitive photodiode sample space
                     , SumDiff, mkSumDiff
                     , sdSum, sdDiff
                     , Psd(Psd, unPsd)
                     , mkPsd
                     , PsdChannels
                       -- * Sensor input space
                     , Sensors(Sensors)
                     , stage, psd
                       -- * Feedback
                     , PropInt(PropInt)
                     , propGain, intGain
                       -- * Convenient re-exports
                     , MonadIO, liftIO
                     ) where
       
import Data.Int
import Data.Foldable
import Data.Traversable
import Data.Functor.Rep
import Control.Applicative       
import Data.Distributive (Distributive, collect)
import Linear       
import Control.Lens hiding (index)
import Control.Monad.IO.Class
import Tracker.Types.Fixed

-- | An ADC sample       
type Sample = Int16

-- | The stage coordinate frame
newtype Stage a = Stage {unStage :: V3 a}
             deriving ( Show, Functor, Foldable, Traversable
                      , Applicative, Additive, Metric
                      , Num, Fractional
                      )

instance Wrapped (Stage a) where
    type Unwrapped (Stage a) = V3 a
    _Wrapped' = iso unStage Stage

instance Distributive Stage where
    collect f = Stage . collect (unStage . f)

instance R1 Stage where _x = _Wrapped' . _x
instance R2 Stage where _y = _Wrapped' . _y
instance R3 Stage where _z = _Wrapped' . _z

instance Representable Stage where
    type Rep Stage = E Stage
    tabulate f = Stage $ V3 (f ex) (f ey) (f ez)
    index v (E e) = v ^. e

mkStage :: a -> a -> a -> Stage a
mkStage x y z = Stage $ V3 x y z
{-# INLINE mkStage #-}

-- | An axis of the stage
data StageAxis = StageX | StageY | StageZ
               deriving (Show, Eq, Ord, Bounded, Enum)

stageAxes :: Stage StageAxis
stageAxes = mkStage StageX StageY StageZ
{-# INLINE stageAxes #-}

-- | A sum-difference sample
newtype SumDiff a = SumDiff {unSumDiff :: V2 a}
                  deriving ( Show, Functor, Foldable, Traversable, Applicative
                           , Additive, Metric, Num, Fractional
                           )

instance Wrapped (SumDiff a) where
    type Unwrapped (SumDiff a) = V2 a
    _Wrapped' = iso unSumDiff SumDiff

instance Distributive SumDiff where
    collect f = SumDiff . collect (unSumDiff . f)

instance R1 SumDiff where _x = _Wrapped' . _x
instance R2 SumDiff where _y = _Wrapped' . _y

instance Representable SumDiff where
    type Rep SumDiff = E SumDiff
    tabulate f = SumDiff $ V2 (f ex) (f ey)
    index v (E e) = v ^. e

mkSumDiff :: a -> a -> SumDiff a
mkSumDiff s d = SumDiff $ V2 s d

sumDiffIso :: Iso' (SumDiff a) (V2 a)
sumDiffIso = iso unSumDiff SumDiff

sdSum, sdDiff :: Lens' (SumDiff a) a
sdSum = sumDiffIso . _x
sdDiff = sumDiffIso . _y

-- | The position-sensitive detector coordinate frame
newtype Psd a = Psd {unPsd :: V2 a}
              deriving ( Show, Functor, Foldable, Traversable, Applicative
                       , Additive, Metric, Num, Fractional
                       )

instance Wrapped (Psd a) where
    type Unwrapped (Psd a) = V2 a
    _Wrapped' = iso unPsd Psd

instance Distributive Psd where
    collect f = Psd . collect (unPsd . f)

instance R1 Psd where _x = _Wrapped' . _x
instance R2 Psd where _y = _Wrapped' . _y

instance Representable Psd where
    type Rep Psd = E Psd
    tabulate f = Psd $ V2 (f ex) (f ey)
    index v (E e) = v ^. e

mkPsd :: a -> a -> Psd a
mkPsd x y = Psd $ V2 x y
{-# INLINE mkPsd #-}

-- | PSD channels with more convenient instances.
-- Construct with 'Wrapped' instance.
newtype PsdChannels a = PsdChan {unPsdChan :: Psd (SumDiff a)}
                      deriving (Show, Functor, Foldable, Traversable)

instance Applicative PsdChannels where
    pure = PsdChan . pure . pure
    PsdChan a <*> PsdChan b = PsdChan $ fmap (<*>) a <*> b

instance Additive PsdChannels where
    zero = pure 0

instance Metric PsdChannels

instance Wrapped (PsdChannels a) where
    type Unwrapped (PsdChannels a) = Psd (SumDiff a)
    _Wrapped' = iso unPsdChan PsdChan

-- | Stage and PSD input
data Sensors a = Sensors { _stage :: !(Stage a)
                         , _psd   :: !(Psd (SumDiff a))
                         }
               deriving (Show, Functor, Foldable, Traversable)
makeLenses ''Sensors

instance Applicative Sensors where
    pure x = Sensors (pure x) (pure $ pure x)
    Sensors s1 p1 <*> Sensors s2 p2 = Sensors (s1 <*> s2) (fmap (<*>) p1 <*> p2)

-- | Proportional and integral gains
data PropInt a = PropInt { _propGain, _intGain :: a }
               deriving (Show, Read, Eq, Ord, Functor, Traversable, Foldable)
makeLenses ''PropInt

instance Applicative PropInt where
    pure x = PropInt x x
    PropInt a b <*> PropInt x y = PropInt (a x) (b y)           
