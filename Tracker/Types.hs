{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveFoldable, DeriveFunctor, DeriveTraversable, TemplateHaskell #-}

module Tracker.Types ( Sample
                     , Stage(..)
                       -- * Modelling position sensitive photodiode
                     , SumDiff(SumDiff)
                     , sdSum, sdDiff
                     , Diode(Diode)
                     , anode, cathode
                     , Psd(Psd, unPsd)
                     , sumDiffDiode
                       -- * Sensor inputs
                     , Sensors(Sensors)
                     , stage, psd
                       -- * Convenient re-exports
                     , MonadIO, liftIO
                     ) where
       
import Data.Int
import Data.Foldable
import Data.Traversable
import Control.Applicative       
import Data.Distributive (Distributive)
import Linear       
import Control.Lens
import Control.Monad.IO.Class

-- | An ADC sample       
type Sample = Int16

-- | The stage coordinate frame
newtype Stage a = Stage {unStage :: V3 a}
             deriving ( Show, Functor, Foldable, Traversable
                      , Applicative, Additive, Metric, Distributive
                      , R1, R2, R3)

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

-- | Stage and PSD input
data Sensors a = Sensors { _stage :: !(Stage a)
                         , _psd   :: !(Psd (SumDiff a))
                         }
               deriving (Show, Functor)
makeLenses ''Sensors

sumDiffDiode :: Num a => Iso' (SumDiff a) (Diode a)
sumDiffDiode = iso to from
    where to (SumDiff sum diff) = Diode (sum - diff) (sum + diff)
          from (Diode an cat) = SumDiff (an - cat) (an + cat)
    
