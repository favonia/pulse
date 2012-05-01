{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}

{#context prefix = "pa"#}

module Sound.Pulse.Internal.Operation where

import Foreign.Storable
import Foreign.Ptr
import Foreign.C.Types
import Sound.Pulse.Internal.C2HS
{#import Sound.Pulse.Internal.Def #}

#include <pulse/operation.h>

data RawOperation
{#pointer *pa_operation as RawOperationPtr -> RawOperation #}

{#fun pa_operation_ref as ^ {id `RawOperationPtr' } -> `RawOperationPtr' id #}

{#fun pa_operation_unref as ^ {id `RawOperationPtr' } -> `()' id #}

{#fun pa_operation_cancel as ^ {id `RawOperationPtr' } -> `()' id #}

{#fun pa_operation_get_state as ^ {id `RawOperationPtr' } -> `Int' #}
