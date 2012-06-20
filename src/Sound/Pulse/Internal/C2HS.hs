{- |
Module      :  Sound.Pulse.Internal.C2HS
License     :  BSD3

Maintainer  :  favonia@gmail.com
Stability   :  experimental
Portability :  non-portable (GHC only)

C2HS marshallers

At this point c2hs users are left writing their own marshalling functions.
Unfortunately marshallers have to be names, not arbitrary expressions.
For the details, see
<http://stackoverflow.com/questions/9471902/whats-the-modern-way-to-access-c2hs-marshalling-functions>

This file was modified from the marshalling library written by
Manuel M T Chakravarty, released under BSD-like license.

-}

--  C->Haskell Compiler: Marshalling library
--
--  Copyright (c) [1999...2005] Manuel M T Chakravarty
--
--  Redistribution and use in source and binary forms, with or without
--  modification, are permitted provided that the following conditions are met:
--
--  1. Redistributions of source code must retain the above copyright notice,
--     this list of conditions and the following disclaimer.
--  2. Redistributions in binary form must reproduce the above copyright
--     notice, this list of conditions and the following disclaimer in the
--     documentation and/or other materials provided with the distribution.
--  3. The name of the author may not be used to endorse or promote products
--     derived from this software without specific prior written permission.
--
--  THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
--  IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
--  OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN
--  NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
--  SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
--  TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
--  PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
--  LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
--  NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
--  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

module Sound.Pulse.Internal.C2HS where

import Foreign.Safe

cIntConv :: (Integral a, Integral b) => a -> b
cIntConv = fromIntegral

cFloatConv :: (RealFloat a, RealFloat b) => a -> b
cFloatConv = realToFrac

cFromBool :: Num a => Bool -> a
cFromBool = fromBool

cToBool :: (Eq a, Num a) => a -> Bool
cToBool = toBool

cToEnum :: (Integral i, Enum e) => i -> e
cToEnum = toEnum . cIntConv

cFromEnum :: (Enum e, Integral i) => e -> i
cFromEnum = cIntConv . fromEnum

combineBitMasks :: (Enum a, Bits b) => [a] -> b
combineBitMasks = foldl (.|.) 0 . map (fromIntegral . fromEnum)
