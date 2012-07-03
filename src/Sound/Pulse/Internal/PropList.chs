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

module Sound.Pulse.Internal.PropList where

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

{#fun proplist_new as ^ {} -> `RawPropListPtr' id #}

{#fun proplist_free as ^ {id `RawPropListPtr' } -> `()' id #}

{#fun proplist_sets as ^ {id `RawPropListPtr', withUTF8CString* `String', withUTF8CString* `String'} -> `Int' #}

{#fun proplist_gets as ^ {id `RawPropListPtr', withUTF8CString* `String'} -> `Maybe String' peekNullableUTF8CString* #}

type PropListIterateState = Ptr ()
{#fun proplist_iterate as ^ {id `RawPropListPtr', id `Ptr PropListIterateState'} -> `Maybe String' peekNullableUTF8CString* #}

{#fun proplist_from_string as ^ {withUTF8CString * `String'} -> `Maybe RawPropListPtr' toMaybePtr #}

