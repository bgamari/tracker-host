module Tracker.Types.Fixed ( -- * 16.16 signed fixed point
                             Fixed16
                           , getFixed16le, putFixed16le
                             -- * 8.24 signed fixed point
                           , Fixed24
                           , getFixed24le, putFixed24le
                           ) where

import Data.Fixed
import Control.Applicative       
import Data.Binary
import Data.Binary.Get (getWord32le)
import Data.Binary.Put (putWord32le)

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


data F24 = F24
-- | A signed 8.24 fixed-point number
type Fixed24 = Fixed F24
                
instance HasResolution F24 where
    resolution _ = 0x1000000
    
getFixed24le :: Get Fixed24
getFixed24le =
    f . fromIntegral <$> getWord32le
  where f :: Fixed24 -> Fixed24
        f x = x / realToFrac (resolution (undefined :: Fixed F24))
{-# INLINE getFixed24le #-}

putFixed24le :: Fixed24 -> Put
putFixed24le a =
    putWord32le $ round $ realToFrac (resolution (0::Fixed F24)) * a
{-# INLINE putFixed24le #-}