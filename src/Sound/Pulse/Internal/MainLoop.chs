{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Safe #-}
#endif
{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}

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

data MainLoop
{#pointer *mainloop as MainLoopPtr -> MainLoop #}

{#fun mainloop_new as ^ {} -> `MainLoopPtr' id #}

{#fun mainloop_free as ^ {id `MainLoopPtr'} -> `()' id #}

{#fun mainloop_prepare as ^ {id `MainLoopPtr', `Int'} -> `Int' #}

{#fun mainloop_poll as ^ {id `MainLoopPtr'} -> `Int' #}

{#fun mainloop_dispatch as ^ {id `MainLoopPtr'} -> `Int' #}

{#fun mainloop_get_retval as ^ {id `MainLoopPtr'} -> `Int' #}

{#fun mainloop_iterate as ^ {id `MainLoopPtr', cFromBool `Bool', withIntConv* `Int' peekIntConv*} -> `Int' cIntConv #}

{#fun mainloop_get_api as ^ {id `MainLoopPtr'} -> `MainLoopApiPtr' id #}

{#fun mainloop_quit as ^ {id `MainLoopPtr', `Int'} -> `()' id #}

{#fun mainloop_wakeup as ^ {id `MainLoopPtr'} -> `()' id #}

