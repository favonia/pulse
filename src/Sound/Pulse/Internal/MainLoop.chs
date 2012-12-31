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
This module provides the bindings to @mainloop.h@.
-}
module Sound.Pulse.Internal.MainLoop where

import Foreign.Safe
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

