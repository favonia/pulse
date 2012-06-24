{-# LANGUAGE Safe #-}
{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}

{#context prefix = "pa"#}

module Sound.Pulse.Internal.PropList where

import Foreign.C
import Foreign.Safe
import Sound.Pulse.Internal.C2HS

#include <pulse/proplist.h>

data RawPropList
{#pointer *proplist as RawPropListPtr -> RawPropList #}

{#fun proplist_new as ^ {} -> `RawPropListPtr' id #}

{#fun proplist_free as ^ {id `RawPropListPtr' } -> `()' id #}

{#fun proplist_sets as ^ {id `RawPropListPtr', withUTF8CString* `String', withUTF8CString* `String'} -> `Int' #}

{#fun proplist_gets as ^ {id `RawPropListPtr', withUTF8CString* `String'} -> `Maybe String' peekNullableUTF8CString* #}

type PropListIterateState = Ptr ()
{#fun proplist_iterate as ^ {id `RawPropListPtr', id `Ptr PropListIterateState'} -> `Maybe String' peekNullableUTF8CString* #}

