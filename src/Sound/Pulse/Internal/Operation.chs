{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Safe #-}
#endif
{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}

{#context prefix = "pa"#}

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

{#fun operation_get_state as ^ {id `RawOperationPtr' } -> `Int' #}
