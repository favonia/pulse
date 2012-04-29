module Sound.Pulse.Internal.C2HS (cIntConv, cFloatConv, cFromBool, cToBool, cToEnum, cFromEnum) where

-- C2HS marshallers
--
-- At this point c2hs users are left writing their own marshalling functions. 
-- Unfortunately marshallers have to be names, not arbitrary expressions.
-- For the details, see 
-- http://stackoverflow.com/questions/9471902/whats-the-modern-way-to-access-c2hs-marshalling-functions 

import Foreign.C.Types
import Foreign.Marshal.Utils

cIntConv :: (Integral a, Integral b) => a -> b
cIntConv = fromIntegral

cFloatConv :: (RealFloat a, RealFloat b) => a -> b
cFloatConv = realToFrac

cFromBool :: Num a => Bool -> a
cFromBool = fromBool

cToBool :: Num a => a -> Bool
cToBool = toBool

cToEnum :: (Integral i, Enum e) => i -> e
cToEnum = toEnum . cIntConv

cFromEnum :: (Enum e, Integral i) => e -> i
cFromEnum = cIntConv . fromEnum
