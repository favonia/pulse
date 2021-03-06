{-
This file is part of Pulse, a Haskell binding to PulseAudio library.

Pulse is free software: you can redistribute it and/or modify it under
BSD-3. You should have received a copy of the BSD-3 License along with
Pulse. If not, see <http://www.opensource.org/licenses/BSD-3-clause>.
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}

{#context prefix = "pa"#}

{- |
This module provides the bindings to @mainloop-api.h@.
-}
module Sound.Pulse.Internal.MainLoopApi where

import Foreign.Safe
import Foreign.C
{#import Sound.Pulse.Internal.TimeVal #}

#include <pulse/mainloop-api.h>

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


data MainLoopApi
{#pointer *mainloop_api as MainLoopApiPtr -> MainLoopApi #}

