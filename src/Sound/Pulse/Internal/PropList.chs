{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Safe #-}
#endif
{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}
{- |
Module      :  Sound.Pulse.Monad
Copyright   :  (c) MnO2
License     :  BSD3

Maintainer  :  mno2.csie@gmail.com
Stability   :  experimental
Portability :  non-portable (GHC only)

This module provides the monadic interface.
-}

{#context prefix = "pa"#}

module Sound.Pulse.Internal.PropList
    (
    RawPropList,
    RawPropListPtr,
    proplistNew,
    proplistFree,
    proplistSet,
    proplistGet,
    PropListIterateState,
    proplistIterate,
    proplistFromString
    ) where

import Control.Exception (throwIO, ErrorCall(..))
import Data.ByteString (ByteString)
#if __GLASGOW_HASKELL__ >= 702
import Foreign.Safe
#else
import Foreign
#endif
import Foreign.C

import Sound.Pulse.Internal.C2HS

#include <pulse/proplist.h>

-- | Raw @PropList@.
data RawPropList

-- | Pointers to raw @PropList@.
{#pointer *proplist as RawPropListPtr -> RawPropList #}

-- | @pa_proplist_new@.
{#fun proplist_new as ^ {} -> `RawPropListPtr' id #}

-- | @pa_proplist_free@.
{#fun proplist_free as ^ {id `RawPropListPtr' } -> `()' id #}

-- | @pa_proplist_set@.

proplistSet :: RawPropListPtr -> String -> ByteString -> IO ()
proplistSet raw key val = do
    ret <- proplistSet' raw key val
    if ret /= 0
        then throwIO $ ErrorCall "invalid key"
        else return ()

{#fun proplist_set as proplistSet' {id `RawPropListPtr', withUTF8CString* `String', useAsCStringLen'* `ByteString'&} -> `Int' #}

-- | @pa_proplist_get@. Throw 'ErrorCall' @"not found"@ if failed.
proplistGet :: RawPropListPtr -> String -> IO ByteString
proplistGet raw key = do
    (ret, cstr, len) <- proplistGet' raw key
    if ret /= 0
        then throwIO $ ErrorCall "not found"
        else packCStringLen (cstr, len)

{#fun proplist_get as proplistGet'
    { id `RawPropListPtr'
    , withUTF8CString* `String'
    , alloca- `CString' castPtr
    , alloca- `Int' peekIntConv*
    } -> `Int' #}

-- | The type of state for 'proplistIterate'.
type PropListIterateState = Ptr ()

-- | @pa_proplist_iterate@.
{#fun proplist_iterate as ^ {id `RawPropListPtr', id `Ptr PropListIterateState'} -> `Maybe String' peekNullableUTF8CString* #}

-- | @pa_proplist_from_string@.
{#fun proplist_from_string as ^ {withUTF8CString * `String'} -> `Maybe RawPropListPtr' toMaybePtr #}

