{-
This file is part of Pulse, a Haskell binding to PulseAudio library.

Pulse is free software: you can redistribute it and/or modify it under
BSD-3. You should have received a copy of the BSD-3 License along with
Pulse. If not, see <http://www.opensource.org/licenses/BSD-3-clause>.
-}

{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Safe #-}
#endif
{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}

{#context prefix = "pa"#}

{- |
This module provides the bindings to @context.h@.
-}
module Sound.Pulse.Internal.Context where

#if __GLASGOW_HASKELL__ >= 702
import Foreign.Safe
#else
import Foreign
#endif
import Foreign.C
import Sound.Pulse.Internal.C2HS
{#import Sound.Pulse.Internal.Def #}
{#import Sound.Pulse.Internal.Operation #}
{#import Sound.Pulse.Internal.MainLoopApi #}
{#import Sound.Pulse.Internal.PropList #}

#include <pulse/context.h>

-- Context Part
type RawContextNotifyCallback a = RawContextPtr -> RawUserData a -> IO ()
type RawContextSuccessCallback a = RawContextPtr -> CInt -> RawUserData a -> IO ()

data RawContext
{#pointer *pa_context as RawContextPtr -> RawContext #}

{#fun context_new_with_proplist as ^ {id `MainLoopApiPtr', withUTF8CString* `String', id `RawPropListPtr'} -> `RawContextPtr' id #}

{#fun context_unref as ^ {id `RawContextPtr'} -> `()' id #}

{#fun context_ref as ^ {id `RawContextPtr'} -> `RawContextPtr' id #}

{#fun context_set_state_callback as ^
    { id `RawContextPtr'
    , id `FunPtr (RawContextNotifyCallback a)'
    , castMaybeStablePtrToPtr `UserData a'
    } -> `()' id #}

{#fun context_is_pending as ^ {id `RawContextPtr'} -> `Int' #}

{#fun context_get_state as ^ {id `RawContextPtr' } -> `ContextState' cToEnum #}

{#fun context_connect as ^
    { id `RawContextPtr'
    , withNullableUTF8CString* `Maybe String'
    , combineBitMasks `[ContextFlags]'
    , id `SpawnApiPtr'
    } -> `Int' #}

{#fun context_disconnect as ^ {id `RawContextPtr' } -> `()' id #}

{#fun context_drain as ^
    { id `RawContextPtr'
    , id `FunPtr (RawContextNotifyCallback a)'
    , castMaybeStablePtrToPtr `UserData a'
    } -> `Maybe RawOperationPtr' toMaybePtr #}

{#fun context_set_default_sink as ^
    { id `RawContextPtr'
    , withUTF8CString* `String'
    , id `FunPtr (RawContextSuccessCallback a)'
    , castMaybeStablePtrToPtr `UserData a'
    } -> `RawOperationPtr' id #}

{#fun context_set_default_source as ^
    { id `RawContextPtr'
    , withUTF8CString* `String'
    , id `FunPtr (RawContextSuccessCallback a)'
    , castMaybeStablePtrToPtr `UserData a'
    } -> `RawOperationPtr' id #}

{#fun context_is_local as ^ {id `RawContextPtr'} -> `Int' #}

{#fun context_set_name as ^
    { id `RawContextPtr'
    , withUTF8CString* `String'
    , id `FunPtr (RawContextSuccessCallback a)'
    , castMaybeStablePtrToPtr `UserData a'
    } -> `RawOperationPtr' id #}

{#fun context_get_server as ^ {id `RawContextPtr'} -> `String' peekUTF8CString* #}

{#fun context_get_protocol_version as ^ {id `RawContextPtr'} -> `Int' #}

{#fun context_get_server_protocol_version as ^ {id `RawContextPtr'} -> `Int' #}
