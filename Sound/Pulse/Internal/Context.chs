{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}

{#context prefix = "pa"#}

module Sound.Pulse.Internal.Context where

import Foreign.Storable
import Foreign.Ptr
import Foreign.C.Types
import Sound.Pulse.Internal.C2HS
{#import Sound.Pulse.Internal.Def #}
{#import Sound.Pulse.Internal.Operation #}
import Sound.Pulse.Internal.PropList

#include <pulse/context.h>
#include <pulse/mainloop-api.h>
#include <pulse/timeval.h>

-- Timeval
data RawTimeVal
{#pointer *timeval as RawTimeValPtr -> RawTimeVal #}

-- MainLoopApi Part; I don't know why a MainLoopApi file won't generate corresponding chi,
-- hence causes compilation fail.
{#enum pa_io_event_flags as IOEventFlags {underscoreToCase} deriving (Show, Eq) #}


data IOEvent
type IOEventPtr = Ptr (IOEvent)
type IOEventCallbackFunction = MainLoopApiPtr -> IOEventPtr -> CInt -> IOEventFlags -> Ptr () -> IO ()
type IOEventDestroyCallbackFunction = MainLoopApiPtr -> IOEventPtr -> Ptr () -> IO ()

data TimeEvent
type TimeEventPtr = Ptr (TimeEvent)
type TimeEventCallbackFunction = MainLoopApiPtr -> TimeEventPtr -> RawTimeValPtr -> Ptr () -> IO ()
type TimeEventDestroyCallbackFunction = MainLoopApiPtr -> TimeEventPtr -> Ptr () -> IO ()

data DeferEvent
type DeferEventPtr = Ptr (DeferEvent)
type DeferEventCallbackFunction = MainLoopApiPtr -> DeferEventPtr -> Ptr () -> IO ()
type DeferEventDestroyCallbackFunction = MainLoopApiPtr -> DeferEventPtr -> Ptr () -> IO ()


type IONewFunction = MainLoopApiPtr -> CInt -> IOEventFlags -> FunPtr (IOEventCallbackFunction) -> Ptr () -> IO (IOEventPtr)
type IOEnableFunction = IOEventPtr -> IOEventFlags -> IO ()
type IOFreeFunction = IOEventPtr -> IO ()
type IOSetDestroyFunction = IOEventPtr -> IOEventDestroyCallbackFunction -> IO ()

type TimeNewFunction = MainLoopApiPtr -> RawTimeValPtr -> FunPtr (TimeEventCallbackFunction) -> Ptr () -> IO (TimeEventPtr) 
type TimeRestartFunction = TimeEventPtr -> RawTimeValPtr -> IO ()
type TimeFreeFunction = TimeEventPtr -> IO ()
type TimeSetDestroyFunction = TimeEventPtr -> TimeEventDestroyCallbackFunction -> IO ()

type DeferNewFunction = MainLoopApiPtr -> FunPtr (DeferEventCallbackFunction) -> Ptr () -> IO (DeferEventPtr)
type DeferEnableFunction = DeferEventPtr -> Int -> IO ()
type DeferFreeFunction = DeferEventPtr -> IO ()
type DeferSetDestroyFunction = DeferEventPtr -> FunPtr (DeferEventDestroyCallbackFunction) -> IO ()

type QuitMainLoopFunction = MainLoopApiPtr -> CInt -> IO ()


data MainLoopApi = MainLoopApi {
                        userData :: Ptr (),
                        ioNew :: FunPtr (IONewFunction),
                        ioEnable :: FunPtr (IOEnableFunction),
                        ioFree :: FunPtr (IOFreeFunction),
                        ioSetDestroy :: FunPtr (IOSetDestroyFunction),
                        timeNew :: FunPtr (TimeNewFunction),
                        timeRestart :: FunPtr (TimeRestartFunction),
                        timeFree :: FunPtr (TimeFreeFunction),
                        timeSetDestroy :: FunPtr (TimeSetDestroyFunction),
                        deferNew :: FunPtr (DeferNewFunction),
                        deferEnable :: FunPtr (DeferEnableFunction),
                        deferFree :: FunPtr (DeferFreeFunction),
                        deferSetDestroy :: FunPtr (DeferSetDestroyFunction),
                        quit :: FunPtr (QuitMainLoopFunction)
                    }   
{#pointer *pa_mainloop_api as MainLoopApiPtr -> MainLoopApi #}


-- Context Part
type RawContextNotifyCallbackFunction = RawContextPtr -> Ptr () -> IO ()
type RawContextSuccessCallbackFunction = RawContextPtr -> CInt -> Ptr () -> IO ()

data RawContext 
{#pointer *pa_context as RawContextPtr -> RawContext #}

{#fun pa_context_new as ^ {id `MainLoopApiPtr', id `Ptr CChar'} -> `RawContextPtr' id #}

{#fun pa_context_unref as ^ {id `RawContextPtr'} -> `()' #}

{#fun pa_context_ref as ^ {id `RawContextPtr'} -> `RawContextPtr' id #}

{#fun pa_context_set_state_callback as ^ {id `RawContextPtr', id `FunPtr RawContextNotifyCallbackFunction', id `Ptr ()'} -> `()' #}

{#fun pa_context_is_pending as ^ {id `RawContextPtr'} -> `Int' #}

{#fun pa_context_get_state as ^ {id `RawContextPtr' } -> `Int' #}

{#fun pa_context_connect as ^ {id `RawContextPtr', id `Ptr CChar', `Int', id `SpawnApiPtr' } -> `Int' #}

{#fun pa_context_disconnect as ^ {id `RawContextPtr' } -> `()' #}

{#fun pa_context_drain as ^ {id `RawContextPtr', id `FunPtr RawContextNotifyCallbackFunction', id `Ptr ()'} -> `RawOperationPtr' id #}

{#fun pa_context_set_default_sink as ^ {id `RawContextPtr', id `Ptr CChar', id `FunPtr RawContextSuccessCallbackFunction', id `Ptr ()'} -> `RawOperationPtr' id #}

{#fun pa_context_set_default_source as ^ {id `RawContextPtr', id `Ptr CChar', id `FunPtr RawContextSuccessCallbackFunction', id `Ptr ()'} -> `RawOperationPtr' id #}

{#fun pa_context_is_local as ^ {id `RawContextPtr'} -> `Int' #}

{#fun pa_context_set_name as ^ {id `RawContextPtr', id `Ptr CChar', id `FunPtr RawContextSuccessCallbackFunction', id `Ptr ()'} -> `RawOperationPtr' id #}

{#fun pa_context_get_server as ^ {id `RawContextPtr'} -> `Ptr CChar' id #}

{#fun pa_context_get_protocol_version as ^ {id `RawContextPtr'} -> `Int' #}

{#fun pa_context_get_server_protocol_version as ^ {id `RawContextPtr'} -> `Int' #}


