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
This module provides the bindings to @operation.h@.
-}
module Sound.Pulse.Internal.Operation where

#if __GLASGOW_HASKELL__ >= 702
import Foreign.Safe
#else
import Foreign
#endif
import Foreign.C
import Sound.Pulse.Internal.C2HS
{#import Sound.Pulse.Internal.Def #}

#include <pulse/operation.h>

data RawOperation
{#pointer *operation as RawOperationPtr -> RawOperation #}

{#fun operation_ref as ^ {id `RawOperationPtr' } -> `RawOperationPtr' id #}

{#fun operation_unref as ^ {id `RawOperationPtr' } -> `()' id #}

{#fun operation_cancel as ^ {id `RawOperationPtr' } -> `()' id #}

{#fun operation_get_state as ^ {id `RawOperationPtr' } -> `OperationState' cToEnum #}
