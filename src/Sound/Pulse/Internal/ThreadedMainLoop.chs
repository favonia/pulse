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

module Sound.Pulse.Internal.ThreadedMainLoop where

#if __GLASGOW_HASKELL__ >= 702
import Foreign.Safe
#else
import Foreign
#endif
import Foreign.C
import Sound.Pulse.Internal.C2HS
{#import Sound.Pulse.Internal.MainLoopApi #}

#include <pulse/thread-mainloop.h>

data ThreadedMainLoop
{#pointer *threaded_mainloop as ThreadedMainLoopPtr -> ThreadedMainLoop #}

{#fun threaded_mainloop_new as ^ {} -> `ThreadedMainLoopPtr' id #}

{#fun threaded_mainloop_free as ^ {id `ThreadedMainLoopPtr'} -> `()' id #}

{#fun threaded_mainloop_start as ^ {id `ThreadedMainLoopPtr'} -> `Int' #}

{#fun threaded_mainloop_stop as ^ {id `ThreadedMainLoopPtr'} -> `()' id #}

{#fun threaded_mainloop_lock as ^ {id `ThreadedMainLoopPtr'} -> `()' id #}

{#fun threaded_mainloop_unlock as ^ {id `ThreadedMainLoopPtr'} -> `()' id #}

{#fun threaded_mainloop_wait as ^ {id `ThreadedMainLoopPtr'} -> `()' id #}

{#fun threaded_mainloop_signal as ^ {id `ThreadedMainLoopPtr', cIntConv `Int' } -> `()' id #}

{#fun threaded_mainloop_accept as ^ {id `ThreadedMainLoopPtr'} -> `()' id #}

{#fun threaded_mainloop_get_retval as ^ {id `ThreadedMainLoopPtr'} -> `Int' #}

{#fun threaded_mainloop_get_api as ^ {id `ThreadedMainLoopPtr'} -> `MainLoopApiPtr' id #}

{#fun threaded_mainloop_in_thread as ^ {id `ThreadedMainLoopPtr'} -> `Int' #}
