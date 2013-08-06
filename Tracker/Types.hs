{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveFoldable, DeriveFunctor, DeriveTraversable, TemplateHaskell #-}

module Tracker.Types where
       
import Data.Int
import Data.Foldable
import Data.Traversable
import Control.Applicative       
import Linear       

-- | An ADC sample       
type Sample = Int16

-- | A stage position
newtype Stage a = Stage {unStage :: V3 a}
             deriving ( Show, Functor, Foldable, Traversable
                      , Applicative, Additive, Metric, R1, R2, R3)

-- | A sum-difference sample
data SumDiff a = SumDiff { sdSum, sdDiff :: !a }
               deriving (Show, Functor, Foldable, Traversable)

instance Applicative SumDiff where
    pure x = SumDiff x x
    SumDiff s d <*> SumDiff x y = SumDiff (s x) (d y)

instance Additive SumDiff where zero = pure 0

-- | Values associated with the anode and cathode of a diode
data Diode a = Diode { anode, cathode :: !a }
             deriving (Show, Functor, Foldable, Traversable)
     
instance Applicative Diode where
    pure x = Diode x x
    Diode a c <*> Diode x y = Diode (a x) (c y)

instance Additive Diode where zero = pure 0

newtype Psd a = Psd {getPsd :: V2 a}
              deriving (Show, Functor, Foldable, Traversable, Applicative, Additive)
