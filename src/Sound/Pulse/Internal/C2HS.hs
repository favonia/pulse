{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Safe #-}
#endif
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

module Sound.Pulse.Internal.C2HS
    ( cIntConv
    , withIntConv
    , peekIntConv
    , cFloatConv
    , cFromBool
    , cToBool
    , cToEnum
    , cFromEnum
    , combineBitMasks
    , nullible
    , nullibleM
    , toMaybePtr
    , peekUTF8CString
    , withUTF8CString
    , withUTF8CStringLen
    , peekNullableUTF8CString
    , withNullableUTF8CString
    , useAsCStringLen
    , useAsCStringLen'
    , packCStringLen
    , UserData
    , RawUserData
    , castMaybeStablePtrToPtr
    , castPtrToMaybeStable
    )
where

import Control.Monad
#if __GLASGOW_HASKELL__ >= 702
import Foreign.Safe
#else
import Foreign
#endif
import Foreign.C
#if __GLASGOW_HASKELL__ >= 702
import qualified GHC.Foreign as GHC
import GHC.IO.Encoding (utf8)
#else
import Codec.Binary.UTF8.String (encode, decode)
#endif
import Data.ByteString (ByteString, useAsCStringLen, packCStringLen)

cIntConv :: (Integral a, Integral b) => a -> b
cIntConv = fromIntegral

withIntConv :: (Storable b, Integral a, Integral b) => a -> (Ptr b -> IO c) -> IO c
withIntConv = with . cIntConv

peekIntConv :: (Storable a, Integral a, Integral b) => Ptr a -> IO b
peekIntConv = liftM cIntConv . peek

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

nullible :: (Ptr a -> b) -> Ptr a -> Maybe b
nullible conv ptr = if ptr == nullPtr then Nothing else Just $ conv ptr

nullibleM :: (Ptr a -> IO b) -> Ptr a -> IO (Maybe b)
nullibleM peeker ptr = if ptr == nullPtr
    then return Nothing
    else liftM Just $ peeker ptr

toMaybePtr :: Ptr a -> Maybe (Ptr a)
toMaybePtr = nullible id

#if __GLASGOW_HASKELL__ >= 702
peekUTF8CString :: CString -> IO String
peekUTF8CString = GHC.peekCString utf8

withUTF8CString :: String -> (CString -> IO a) -> IO a
withUTF8CString = GHC.withCString utf8

withUTF8CStringLen :: String -> (CStringLen -> IO a) -> IO a
withUTF8CStringLen = GHC.withCStringLen utf8
#else
nul :: CChar
nul = 0

peekUTF8CString :: CString -> IO String
peekUTF8CString = liftM (decode . map cIntConv) . peekArray0 nul

withUTF8CString :: String -> (CString -> IO a) -> IO a
withUTF8CString = withArray0 nul . map cIntConv . encode

withUTF8CStringLen :: String -> (CStringLen -> IO a) -> IO a
withUTF8CStringLen str = withArrayLen (map cIntConv . encode $ str) . flip . curry
#endif

peekNullableUTF8CString :: CString -> IO (Maybe String)
peekNullableUTF8CString = nullibleM peekUTF8CString

withNullableUTF8CString :: Maybe String -> (CString -> IO a) -> IO a
withNullableUTF8CString Nothing = ($ nullPtr)
withNullableUTF8CString (Just s) = withUTF8CString s

useAsCStringLen' :: Integral c => ByteString -> ((Ptr b, c) -> IO a) -> IO a
useAsCStringLen' bstr code = useAsCStringLen bstr $ \(str, len) -> code (castPtr str, cIntConv len)

type UserData a = Maybe (StablePtr a)
type RawUserData a = Ptr ()

castMaybeStablePtrToPtr :: Maybe (StablePtr a) -> Ptr ()
castMaybeStablePtrToPtr Nothing = nullPtr
castMaybeStablePtrToPtr (Just p) = castStablePtrToPtr p

castPtrToMaybeStable :: Ptr () -> Maybe (StablePtr a)
castPtrToMaybeStable = nullible castPtrToStablePtr
