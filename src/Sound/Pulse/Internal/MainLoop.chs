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

module Sound.Pulse.Internal.MainLoop where

#if __GLASGOW_HASKELL__ >= 702
import Foreign.Safe
#else
import Foreign
#endif
import Foreign.C
import Sound.Pulse.Internal.C2HS
{#import Sound.Pulse.Internal.MainLoopApi #}

#include <pulse/mainloop.h>

data RawMainLoop
{#pointer *mainloop as RawMainLoopPtr -> RawMainLoop #}

{#fun mainloop_new as ^ {} -> `RawMainLoopPtr' id #}

{#fun mainloop_free as ^ {id `RawMainLoopPtr'} -> `()' id #}

{#fun mainloop_prepare as ^ {id `RawMainLoopPtr', `Int'} -> `Int' #}

{#fun mainloop_poll as ^ {id `RawMainLoopPtr'} -> `Int' #}

{#fun mainloop_dispatch as ^ {id `RawMainLoopPtr'} -> `Int' #}

{#fun mainloop_get_retval as ^ {id `RawMainLoopPtr'} -> `Int' #}

{#fun mainloop_iterate as ^ {id `RawMainLoopPtr', cFromBool `Bool', withIntConv* `Int' peekIntConv*} -> `Int' cIntConv #}

{#fun mainloop_get_api as ^ {id `RawMainLoopPtr'} -> `MainLoopApiPtr' id #}

{#fun mainloop_quit as ^ {id `RawMainLoopPtr', `Int'} -> `()' id #}

{#fun mainloop_wakeup as ^ {id `RawMainLoopPtr'} -> `()' id #}

