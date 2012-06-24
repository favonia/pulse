{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}

{#context prefix = "pa"#}

module Sound.Pulse.Internal.ThreadedMainLoop where

import Foreign.Safe
import Foreign.C
import Sound.Pulse.Internal.C2HS

#include <pulse/thread-mainloop.h>

data ThreadedMainLoop
{#pointer *threaded_mainloop as ThreadedMainLoopPtr -> ThreadedMainLoop #}

{#fun threaded_mainloop_new as ^ {} -> `ThreadedMainLoopPtr' id #}

{#fun threaded_mainloop_free as ^ {id `ThreadedMainLoopPtr'} -> `()' #}

{#fun threaded_mainloop_start as ^ {id `ThreadedMainLoopPtr'} -> `Int' #}

{#fun threaded_mainloop_stop as ^ {id `ThreadedMainLoopPtr'} -> `()' #}

{#fun threaded_mainloop_lock as ^ {id `ThreadedMainLoopPtr'} -> `()' #}

{#fun threaded_mainloop_unlock as ^ {id `ThreadedMainLoopPtr'} -> `()' #}

{#fun threaded_mainloop_wait as ^ {id `ThreadedMainLoopPtr'} -> `()' #}

{#fun threaded_mainloop_signal as ^ {id `ThreadedMainLoopPtr', cIntConv `Int' } -> `()' #}

{#fun threaded_mainloop_accept as ^ {id `ThreadedMainLoopPtr'} -> `()' #}

{#fun threaded_mainloop_get_retval as ^ {id `ThreadedMainLoopPtr'} -> `Int' #}

{#fun threaded_mainloop_in_thread as ^ {id `ThreadedMainLoopPtr'} -> `Int' #}
