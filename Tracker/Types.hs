{-# LANGUAGE DeriveFoldable, DeriveFunctor, DeriveTraversable #-}

module Tracker.Types where
       
import Data.Int
import Data.Foldable
import Data.Traversable
import Control.Applicative       

-- | An ADC sample       
type Sample = Int16

-- | A stage position
data Stage a = Stage !a !a !a
             deriving (Show, Functor, Foldable, Traversable)

instance Applicative Stage where
    pure x = Stage x x x
    Stage a b c <*> Stage x y z = Stage (a x) (b y) (c z)
    
-- | An PSD reading
data Psd a = Psd { psdDiffX, psdDiffY :: !a
                 , psdSumX, psdSumY   :: !a
                 }
             deriving (Show, Functor, Foldable, Traversable)

instance Applicative Psd where
    pure x = Psd x x x x
    Psd a b c d <*> Psd w x y z = Psd (a w) (b x) (c y) (d z)
     
-- | A frame of ADC samples
type Frame = (Stage Sample, Psd Sample)
