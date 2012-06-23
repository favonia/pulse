{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Safe #-}
#endif
{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}

{#context prefix = "pa"#}

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
import Sound.Pulse.Internal.PropList

#include <pulse/context.h>
#include <pulse/mainloop-api.h>
#include <pulse/timeval.h>

-- Timeval
data RawTimeVal
{#pointer *timeval as RawTimeValPtr -> RawTimeVal #}

-- MainLoopApi Part; I don't know why a MainLoopApi file won't generate corresponding chi,
-- hence causes compilation fail.
{#enum io_event_flags as IOEventFlags {underscoreToCase} deriving (Show, Eq) #}


data IOEvent
type IOEventPtr = Ptr (IOEvent)
type IOEventCallback = MainLoopApiPtr -> IOEventPtr -> CInt -> IOEventFlags -> Ptr () -> IO ()
type IOEventDestroyCallback = MainLoopApiPtr -> IOEventPtr -> Ptr () -> IO ()

data TimeEvent
type TimeEventPtr = Ptr (TimeEvent)
type TimeEventCallback = MainLoopApiPtr -> TimeEventPtr -> RawTimeValPtr -> Ptr () -> IO ()
type TimeEventDestroyCallback = MainLoopApiPtr -> TimeEventPtr -> Ptr () -> IO ()

data DeferEvent
type DeferEventPtr = Ptr (DeferEvent)
type DeferEventCallback = MainLoopApiPtr -> DeferEventPtr -> Ptr () -> IO ()
type DeferEventDestroyCallback = MainLoopApiPtr -> DeferEventPtr -> Ptr () -> IO ()


type IONewFunction = MainLoopApiPtr -> CInt -> IOEventFlags -> FunPtr IOEventCallback -> Ptr () -> IO IOEventPtr
type IOEnableFunction = IOEventPtr -> IOEventFlags -> IO ()
type IOFreeFunction = IOEventPtr -> IO ()
type IOSetDestroyFunction = IOEventPtr -> IOEventDestroyCallback -> IO ()

type TimeNewFunction = MainLoopApiPtr -> RawTimeValPtr -> FunPtr TimeEventCallback -> Ptr () -> IO TimeEventPtr
type TimeRestartFunction = TimeEventPtr -> RawTimeValPtr -> IO ()
type TimeFreeFunction = TimeEventPtr -> IO ()
type TimeSetDestroyFunction = TimeEventPtr -> TimeEventDestroyCallback -> IO ()

type DeferNewFunction = MainLoopApiPtr -> FunPtr DeferEventCallback -> Ptr () -> IO DeferEventPtr
type DeferEnableFunction = DeferEventPtr -> Int -> IO ()
type DeferFreeFunction = DeferEventPtr -> IO ()
type DeferSetDestroyFunction = DeferEventPtr -> FunPtr DeferEventDestroyCallback -> IO ()

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
{#pointer *mainloop_api as MainLoopApiPtr -> MainLoopApi #}


-- Context Part
type RawContextNotifyCallback a = RawContextPtr -> RawUserData a -> IO ()
type RawContextSuccessCallback a = RawContextPtr -> CInt -> RawUserData a -> IO ()

data RawContext
{#pointer *pa_context as RawContextPtr -> RawContext #}

{#fun context_new as ^ {id `MainLoopApiPtr', withUTF8CString* `String'} -> `RawContextPtr' id #}

{#fun context_unref as ^ {id `RawContextPtr'} -> `()' #}

{#fun context_ref as ^ {id `RawContextPtr'} -> `RawContextPtr' id #}

{#fun context_set_state_callback as ^
    { id `RawContextPtr'
    , id `FunPtr (RawContextNotifyCallback a)'
    , castMaybeStablePtrToPtr `UserData a'
    } -> `()' #}

{#fun context_is_pending as ^ {id `RawContextPtr'} -> `Int' #}

{#fun context_get_state as ^ {id `RawContextPtr' } -> `Int' #}

{#fun context_connect as ^
    { id `RawContextPtr'
    , withNullableUTF8CString* `Maybe String'
    , combineBitMasks `[ContextFlags]'
    , id `SpawnApiPtr'
    } -> `Int' #}

{#fun context_disconnect as ^ {id `RawContextPtr' } -> `()' #}

{#fun context_drain as ^
    { id `RawContextPtr'
    , id `FunPtr (RawContextNotifyCallback a)'
    , castMaybeStablePtrToPtr `UserData a'
    } -> `RawOperationPtr' id #}

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
