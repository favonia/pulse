{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}

{#context prefix = "pa"#}

module Sound.Pulse.Internal.Context where

import Foreign.Storable
import Foreign.Ptr
import Foreign.C.Types
import Sound.Pulse.Internal.C2HS
{#import Sound.Pulse.Internal.Def #}
{#import Sound.Pulse.Internal.Operation #}

#include <pulse/context.h>

type RawContextNotifyCallbackFunction = RawContextPtr -> Ptr () -> IO ()
type RawContextSuccessCallbackFunction = RawContextPtr -> CInt -> Ptr () -> IO ()

data RawContext 
{#pointer *pa_context as RawContextPtr -> RawContext #}

{#fun pa_context_connect as ^ {id `RawContextPtr', id `Ptr CChar', `Int', id `SpawnApiPtr' } -> `Int' #}

{#fun pa_context_disconnect as ^ {id `RawContextPtr' } -> `()' #}

{#fun pa_context_get_state as ^ {id `RawContextPtr' } -> `Int' #}

{#fun pa_context_drain as ^ {id `RawContextPtr', id `FunPtr RawContextNotifyCallbackFunction', id `Ptr ()'} -> `RawOperationPtr' id #}
